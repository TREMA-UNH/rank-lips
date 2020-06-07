{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module MultiTrecRunFile where

import Control.Parallel.Strategies
import Control.DeepSeq
import GHC.Generics
import qualified Data.DList as DList
import Data.Foldable
import Data.Ord
import Data.List
import Data.Monoid
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified CAR.RunFile as RunFile

import qualified Debug.Trace as Debug


type Score = Double
type Rank = Int
type QueryId = RunFile.QueryId

type RankingEntry doc = RunFile.RankingEntry' doc -- CAR.RunFile definition


-- Compound type for storing results of transposing multiple rankings over doc,
-- into a doc -> [ranking], i.e., the "all".
-- It also exposes the collapsed version by unsupervised rank aggregation (score(doc) = sum_rankings 1/rank(doc))
data MultiRankingEntry doc key = MultiRankingEntry { multiRankingEntryCollapsed :: !(RankingEntry doc)
                                                   , multiRankingEntryAll       :: [(key, RankingEntry doc)]
                                                   }
                               deriving (Generic, Show)
instance (NFData doc, NFData key) => NFData (MultiRankingEntry doc key)

multiRankingEntryGetDocumentName :: MultiRankingEntry doc key -> doc
multiRankingEntryGetDocumentName mre =  RunFile.carDocument $ multiRankingEntryCollapsed mre


multiAsRankingEntry :: MultiRankingEntry doc key -> RankingEntry doc
multiAsRankingEntry multiRankingEntry = multiRankingEntryCollapsed multiRankingEntry

-- | Given a list of runs, compute the "collapsed" ranking that would result
-- summing the reciprocol rank of each item under all rankings.
collapseRuns :: forall doc key. (Eq doc, Hashable doc, Hashable key, Show key, Show doc)
             => [(key, [RankingEntry doc])]
             -> M.Map QueryId [MultiRankingEntry doc key]
collapseRuns runs =
    if any (\(k, r) -> identicalScoreRanking r) runs then
        error $ "MultiTrecRunFile collapsing run with identical scores :"
                 <> show [ k | (k,r) <- runs, identicalScoreRanking r]
    else
       let listOfRunMaps :: M.Map QueryId [(key, RankingEntry doc)]
           listOfRunMaps = fmap toList $ RunFile.groupByQuery' [ (key, elem)
                                                                | (key, run) <- runs
                                                                , elem <- run
                                                                ]

         in M.fromAscList
            $ withStrategy (parBuffer 100 (evalTuple2 r0 rseq))
            [ (query, (collapseRankings rankings))
            | (query, rankings) <- M.toAscList listOfRunMaps  -- fetch rankings for query
            ]

identicalScoreRanking :: [RankingEntry doc] -> Bool
identicalScoreRanking ranking =
    if null ranking then True else
        let (anyElem : _) = ranking
            anyScore = RunFile.carScore anyElem
        in not $ any (\entry -> anyScore /= (RunFile.carScore entry) ) ranking

-- rankings: rank entries for the same query, across different run files
collapseRankings :: forall doc key. (Eq doc, Hashable doc)
                 => [(key, RankingEntry doc)]
                 -> [MultiRankingEntry doc key]
collapseRankings rankingEntries =
    -- docid -> [ entries ]
    -- groupBy (docId) rankingEntries
    let groupByDoc :: [DList.DList (key, RankingEntry doc)]
        groupByDoc = HM.elems -- equivalent for HM.values
                   $ HM.fromListWith (<>)
                   $ [ ((RunFile.carDocument entry), DList.singleton (key, entry))
                     | (key, entry) <- rankingEntries
                     ]

        -- with score but not sorted
        scoredMultiRankings :: [ (Score,  [(key, RankingEntry doc)]) ]
        scoredMultiRankings = [ (aggregatedScore rankingsPerDoc', rankingsPerDoc')
                              | rankingsPerDoc <- groupByDoc
                              , let !rankingsPerDoc' = toList rankingsPerDoc
                              ]


        -- sorted and with ranks
    in zipWith buildMultiTrecEntry [1 ..]
       $ sortOn (Down . fst) scoredMultiRankings

buildMultiTrecEntry :: Rank -> (Score, [(key, RankingEntry doc)]) -> MultiRankingEntry doc key
buildMultiTrecEntry r (s, rankings) =
            MultiRankingEntry
                { multiRankingEntryCollapsed =
                      RunFile.RankingEntry { RunFile.carQueryId = qId
                                           , RunFile.carDocument = docName
                                           , RunFile.carRank = r
                                           , RunFile.carScore = s
                                           , RunFile.carMethodName = RunFile.MethodName "collapsed"
                                           }
                , multiRankingEntryAll = rankings
                }
          where
            qId = RunFile.carQueryId $ snd $ head rankings
            docName = RunFile.carDocument $ snd $ head rankings



-- precondition: all RankingEntry share the same queryId
-- precondition2: all RankingEntry share the same documentName
aggregatedScore :: [(key, RankingEntry doc)] -> Score
aggregatedScore rankings =
    recipRankAggregation $ fmap  ( RunFile.carRank . snd) rankings
  where
    recipRankAggregation :: [Rank] -> Score
    recipRankAggregation rs =
        sum $ fmap recipRank rs
      where
          recipRank :: Int -> Double
          recipRank x | x < 0 = error $ "rank needs to be positive but is "++ (show x)
          recipRank r = 1.0/(realToFrac r+1)

--  Todo: compare or merge with MergeRankings.hs
