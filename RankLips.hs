{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Control.Concurrent.Async
import Control.DeepSeq hiding (rwhnf)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad
import Control.Parallel.Strategies
import Control.Lens (each)
import Data.Coerce
import Data.Tuple
import Data.Semigroup hiding (All, Any, option)
import Options.Applicative
import System.IO
import Data.Aeson
import System.Random
import GHC.Generics
import GHC.Stack
import Control.Exception
import System.FilePath

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as ML
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Text.PrettyPrint.Leijen.Text as PP
import Data.List
import Data.Maybe
import Data.Foldable as Foldable
import Data.Hashable
import Control.Concurrent
import Control.Concurrent.Map
import Data.List.Split
import qualified Codec.Serialise as CBOR
import qualified SimplIR.Format.TrecRunFile as SimplirRun



-- import CAR.Retrieve as Retrieve
-- import qualified CAR.RunFile as CarRun
-- import CAR.Utils
-- import GridFeatures

import qualified SimplIR.SimpleIndex as Index
import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (FeatureSpace, FeatureVec)
import SimplIR.FeatureSpace.Normalise
import SimplIR.Intern

-- import qualified CAR.RunFile as CAR.RunFile
-- todo switch to import qualified SimplIR.Format.TrecRunFile as Simplir.RunFile
import qualified SimplIR.Format.QRel as QRel
import qualified SimplIR.Ranking as Ranking
-- import MultiTrecRunFile

import Control.Monad

-- import GridFeatures
import TrainAndSave

import Debug.Trace  as Debug
import CAR.Types (ParagraphId)
import CAR.ToolVersion (execParser')

type NumResults = Int


newtype Feat = Feat { featureName :: T.Text }
    deriving (Eq, Show, Ord, Read)

data FeatureVariant = FeatScore   
    deriving (Eq, Show, Ord,  Read, Enum, Bounded)  -- Todo: Hashable
-- | FeatRecipRank

type RankEntry = SimplirRun.DocumentName


includeCv = False

-- startDir = "/home/dietz/trec-car/code/mediawiki-annotate/data/rank-lips"
-- startDir = "/home/dietz/trec-car/code/mediawiki-annotate/data/eal-single"
-- startDir = "/home/dietz/trec-car/code/mediawiki-annotate/data/eal-tools"

-- dirPrefix = startDir </> "features"
outputFilePrefix :: FilePath
outputFilePrefix =  ""

miniBatchParams :: MiniBatchParams
miniBatchParams = defaultMiniBatchParams

opts :: Parser (IO ())
opts = subparser
    $  cmd "train"        doTrain'
  where
    cmd name action = command name (info (helper <*> action) fullDesc)

    doTrain' =
        f <$> option str (long "feature-runs-directory" <> short 'd' <> help "directory containing run files for features" <> metavar "DIR")
          <*> option str (long "qrels" <> short 'q' <> help "qrels file used for training" <> metavar "QRELS" )
          <*> option str (long "model" <> short 'm' <> help "file where model parameters will be written to" <> metavar "FILE" )
          <*> many (option str (long "feature" <> short 'f' <> help "feature name, needs to match filename in feature-runs-directory" <> metavar "FEATURE") )
      where
        f featureRunsDirectory qrelFile modelFile features = 
            doTrain featureRunsDirectory qrelFile modelFile features 



doTrain :: FilePath -> FilePath -> FilePath -> [FilePath] ->  IO ()
doTrain featureRunsDirectory qrelFile modelFile runFilenames = do

    let updatedModelFile = modelFile <> ".json"
        (MiniBatchParams batchSteps batchSize evalSteps) = miniBatchParams
        evalSteps' = evalSteps +2


    qrelData <- QRel.readQRel qrelFile
                :: IO [QRel.Entry SimplirRun.QueryId SimplirRun.DocumentName IsRelevant]

    let qrelMap :: M.Map SimplirRun.QueryId (M.Map SimplirRun.DocumentName IsRelevant)
        qrelMap = M.fromListWith (<>)
                [ (queryId, M.singleton documentName relevance)
                | QRel.Entry {..}  <- qrelData 
                ]

        lookupQrel :: IsRelevant -> SimplirRun.QueryId -> SimplirRun.DocumentName -> IsRelevant
        lookupQrel defaultRel queryId docName =
            case queryId `M.lookup` qrelMap of
                Nothing -> defaultRel
                Just dMap -> case  docName `M.lookup` dMap of
                                Nothing -> defaultRel
                                Just r -> r

 
    -- let runFilenames :: [FilePath]
        -- runFilenames = ["PARA_CONTENT_TO_ASPECT_HEADER_TFIDF", "SENT_CONTENT_TO_ASPECT_HEADER_BM25", "SIMP_SENT_ENTITIES_OVERLAP"]
        -- runFilenames = ["f1","f2","f3"]

    let
        featureNames :: S.Set Feat
        featureNames = S.fromList $ [ augmentFname ft run 
                                    | run <-  runFilenames
                                    , ft <- [minBound @FeatureVariant .. maxBound @FeatureVariant]
                                    ]

    F.SomeFeatureSpace (fspace:: F.FeatureSpace Feat ph) <- pure $ F.mkFeatureSpace featureNames

    runFiles <- loadRunFiles  featureRunsDirectory runFilenames
    putStrLn $ " loadRunFiles " <> (unwords $ fmap fst runFiles)

    let featureDataMap = runFilesToFeatureVectorsMap fspace runFiles
        featureDataList = fmap M.toList featureDataMap


        allDataMap :: M.Map SimplirRun.QueryId (M.Map SimplirRun.DocumentName  (FeatureVec Feat ph Double, Rel))
        allDataMap = augmentWithQrelsMap_ (lookupQrel NotRelevant) featureDataMap

        allDataList :: M.Map SimplirRun.QueryId [( SimplirRun.DocumentName, FeatureVec Feat ph Double, Rel)]
        allDataList = augmentWithQrelsList_ (lookupQrel NotRelevant) featureDataList

        totalRels :: M.Map SimplirRun.QueryId Int
        !totalRels = fmap countRel qrelMap
                    where countRel :: M.Map x IsRelevant -> Int
                          countRel m = length [ r
                                                | (_, r) <- M.toList m
                                                , QRel.isPositive r
                                                ]

        metric :: ScoringMetric IsRelevant SimplirRun.QueryId
        metric = meanAvgPrec (\q -> fromMaybe 0 $ q `M.lookup` totalRels)  Relevant

        -- !someKindOfTrainingData = M.fromList $ [(q,q) | q <- intersect (M.keys totalRels) (M.keys featureGraphs) ] -- totalRels does not include queries for which there is no training data

    gen0 <- newStdGen

    train includeCv fspace allDataList qrelData miniBatchParams outputFilePrefix modelFile


train :: Bool
      -> F.FeatureSpace Feat s
      -> TrainData Feat s
      -> [QRel.Entry SimplirRun.QueryId doc IsRelevant]
      -> MiniBatchParams
      -> FilePath
      -> FilePath
      -> IO()
train includeCv fspace allData qrel miniBatchParams outputFilePrefix modelFile =  do
              let metric :: ScoringMetric IsRelevant SimplirRun.QueryId
                  !metric = meanAvgPrec (totalRelevantFromQRels qrel) Relevant
                  totalElems = getSum . foldMap ( Sum . length ) $ allData
                  totalPos = getSum . foldMap ( Sum . length . filter (\(_,_,rel) -> rel == Relevant)) $ allData

              putStrLn $ "Feature dimension: "++show (F.dimension $ F.featureSpace $ (\(_,a,_) -> a) $ head' $ snd $ M.elemAt 0 allData)
              putStrLn $ "Training model with (trainData) "++ show (M.size allData) ++
                        " queries and "++ show totalElems ++" items total of which "++
                        show totalPos ++" are positive."
              let
                  displayTrainData :: Show f => TrainData f s -> [String]
                  displayTrainData trainData =
                    [ show k ++ " " ++ show d ++ " " ++ show r ++ " -> "++ prettyFv
                    | (k,list) <- M.toList trainData
                    , (d,fvec, r) <- list
                    , let prettyFv = unlines $ fmap show $ F.toList fvec
                    ]

              putStrLn $ "Training Data = \n" ++ intercalate "\n" (take 10 $ displayTrainData $ force allData)
              gen0 <- newStdGen  -- needed by learning to rank
              trainMe includeCv miniBatchParams (EvalCutoffAt 100) gen0 allData fspace metric outputFilePrefix modelFile



augmentFname :: FeatureVariant -> FilePath -> Feat
augmentFname featureVariant fname = Feat $ T.pack $ fname <> "-" <> show featureVariant

loadRunFiles :: FilePath -> [FilePath] ->  IO [(FilePath, [SimplirRun.RankingEntry])] 
loadRunFiles prefix inputRuns = do
    mapM (\fname -> (fname,) <$> SimplirRun.readRunFile (prefix </> fname)) inputRuns    
                

augmentWithQrelsMap_ :: (SimplirRun.QueryId -> SimplirRun.DocumentName -> Rel) 
                 -> M.Map SimplirRun.QueryId (M.Map SimplirRun.DocumentName (F.FeatureVec Feat ph Double)) 
                 -> M.Map SimplirRun.QueryId (M.Map SimplirRun.DocumentName (F.FeatureVec Feat ph Double, Rel))
augmentWithQrelsMap_ lookupQrel qData =
    M.mapWithKey mapQueries qData
  where mapQueries :: SimplirRun.QueryId -> (M.Map SimplirRun.DocumentName d) -> M.Map SimplirRun.DocumentName (d, Rel)
        mapQueries query dMap =
            M.mapWithKey mapDocs dMap
          where mapDocs :: SimplirRun.DocumentName -> d -> (d,Rel)
                mapDocs doc feats =
                    (feats, lookupQrel query doc)


augmentWithQrelsList_ :: (SimplirRun.QueryId -> SimplirRun.DocumentName -> Rel) 
                 -> M.Map SimplirRun.QueryId [( SimplirRun.DocumentName, (F.FeatureVec Feat ph Double))] 
                 -> M.Map SimplirRun.QueryId [(SimplirRun.DocumentName, F.FeatureVec Feat ph Double, Rel)]
augmentWithQrelsList_ lookupQrel qData =
    M.mapWithKey mapQueries qData
  where mapQueries :: SimplirRun.QueryId -> [(SimplirRun.DocumentName, d)] -> [(SimplirRun.DocumentName, d, Rel)]
        mapQueries query dMap =
            fmap mapDocs dMap
          where mapDocs :: (SimplirRun.DocumentName, d) -> (SimplirRun.DocumentName, d,Rel)
                mapDocs (doc, feats) =
                    (doc, feats, lookupQrel query doc)


runFilesToFeatureVectorsMap :: forall ph . F.FeatureSpace Feat ph 
                          -> [(FilePath, [SimplirRun.RankingEntry])] 
                          -> M.Map SimplirRun.QueryId (M.Map SimplirRun.DocumentName (F.FeatureVec Feat ph Double))
runFilesToFeatureVectorsMap fspace runData = 
    let features:: M.Map SimplirRun.QueryId (M.Map SimplirRun.DocumentName [(Feat, Double)]) -- FeatureVec Feat ph Double)]
        features = M.fromListWith (M.unionWith (<>))
                 $ [ (queryId, 
                        M.fromListWith (<>)
                        [(documentName 
                         ,  [ ((augmentFname FeatScore fname), documentScore)
                            -- , ((augmentFname FeatRecipRank fname), (1.0/(realToFrac documentRank)))  
                            ]
                        )]
                      )
                    | (fname, rankingEntries) <- runData
                    , SimplirRun.RankingEntry {..} <- rankingEntries
                    ]
                    -- ToDo Where are default values handled?

        featureVectors :: M.Map SimplirRun.QueryId (M.Map SimplirRun.DocumentName (F.FeatureVec Feat ph Double))
        featureVectors =  fmap featureVectorize features           

    in featureVectors
  where featureVectorize :: M.Map SimplirRun.DocumentName [(Feat, Double)] -> M.Map SimplirRun.DocumentName (FeatureVec Feat ph Double)
        featureVectorize docFeatureList =
            fmap (F.fromList fspace)  docFeatureList


--   where makeFeatureVectors ::  [(Feat, Double)] -> FeatureVec Feat ph Double
--         makeFeatureVectors featureList = F.fromList fspace featureList


-- data RankingEntry = RankingEntry { queryId       :: !QueryId
--                                  , documentName  :: !DocumentName
--                                  , documentRank  :: !Rank
--                                  , documentScore :: !Score
--                                  , methodName    :: !MethodName
--                                  }
--                   deriving (Show)



head' :: HasCallStack => [a] -> a
head' (x:_) = x
head' [] = error $ "head': empty list"




main :: IO ()
main = join $ execParser' 1 (helper <*> opts) mempty
