{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module TrainAndSave where

import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
import qualified Data.Map.Strict as M
-- import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.List
import Data.Foldable as Foldable
-- import Data.Maybe

import Data.Function
import Data.Bifunctor
import System.Random
import Control.Parallel.Strategies
import Control.Concurrent.Map
import Control.DeepSeq

import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import SimplIR.FeatureSpace (FeatureSpace, FeatureVec)

-- import qualified SimplIR.Format.QRel as QRel
import qualified SimplIR.Ranking as Ranking
import SimplIR.TrainUtils

import Debug.Trace as Debug

import qualified SimplIR.Format.TrecRunFile as SimplirRun
-- import GHC.Generics (Generic)
-- import GHC.Stack (HasCallStack)
-- import Data.Functor.Contravariant (Contravariant(contramap))

import RankLipsTypes
import Data.Maybe
import GHC.Base (when)

type ModelEnvelope f s = Model f s -> RankLipsModelSerialized f


-- --------------------------------------------

-- trainMe :: forall f s q d. (Ord f, ToJSONKey f, Show f, NFData q, Ord q, Show q, Show d, Render q, Render d)
--         => Bool
--         -> MiniBatchParams
--         -> ConvergenceDiagParams
--         -> StdGen
--         -> TrainData f s q d
--         -> FeatureSpace f s
--         -> ScoringMetric IsRelevant q
--         -> FilePath
--         -> FilePath
--         -> (Maybe Integer -> Maybe [q] -> Model f s -> RankLipsModelSerialized f)
--         -> IO ()
-- trainMe includeCv miniBatchParams convDiagParams gen0 trainData fspace metric outputFilePrefix experimentName modelEnvelope = do
--           -- train me!
--           let nRestarts = convergenceRestarts convDiagParams
--               nFolds = convergenceFolds convDiagParams


--           -- folded CV
--                                 -- todo load external folds
--               !folds = force $ mkSequentialFolds nFolds (M.keys trainData)
--           putStrLn "made folds"
--           putStrLn $ unwords [ show $ length f | f <- getFolds folds ]

--           let trainFun :: FoldIdx -> TrainData f s q d -> [(Model f s, Double)]
--               trainFun foldIdx =
--                   trainWithRestarts nRestarts miniBatchParams convDiagParams gen0 metric infoStr fspace
--                 where
--                   infoStr = show foldIdx

--               foldRestartResults :: Folds (M.Map  q [(d, FeatureVec f s Double, Rel)], [(Model f s, Double)])
--               foldRestartResults = trainKFolds trainFun trainData folds

--               strat :: Strategy (Folds (a, [(Model f s, Double)]))
--               strat = parTraversable (evalTuple2 r0 (parTraversable rdeepseq))
          
--           putStrLn "full Train"
--           -- full train
--           let fullRestarts = withStrategy (parTraversable rdeepseq)
--                              $ trainWithRestarts nRestarts miniBatchParams convDiagParams gen0 metric "full" fspace trainData
--               (model, trainScore) =  bestModel $  fullRestarts
--               fullActions = fmap (storeModelAndRanking outputFilePrefix experimentName modelEnvelope ) $ dumpFullModelsAndRankings trainData (model, trainScore) metric


--           putStrLn "CV Train"
--           if includeCv
--             then do
--               foldRestartResults' <- withStrategyIO strat foldRestartResults
--               let cvActions = fmap (storeModelAndRanking outputFilePrefix experimentName modelEnvelope) $  dumpKFoldModelsAndRankings foldRestartResults' metric 
--               putStrLn "concurrently: CV Train"
--               mapConcurrentlyL_ 24 id $ fullActions ++ cvActions
--             else
--               mapConcurrentlyL_ 24 id $ fullActions
--           putStrLn "dumped all models and rankings"



trainWithRestarts
    :: forall f s q d . (ToJSONKey f, Show f, NFData q, Ord q, Show q, Show d, Render q, Render d)
    => Int
    -> MiniBatchParams
    -> ConvergenceDiagParams
    -> StdGen
    -> ScoringMetric IsRelevant q
    -> String
    -> FeatureSpace f s
    -> TrainData f s q d
    -> [(Model f s, Double)]
       -- ^ an infinite list of restarts
trainWithRestarts nRestarts miniBatchParams (ConvergenceDiagParams convThreshold convMaxIter convDropIter evalCutoff _numRestarts _numFolds) gen0 metric info fspace trainData =
  let trainData' = discardUntrainable trainData

      rngSeeds :: [StdGen]
      rngSeeds = unfoldr (Just . System.Random.split) gen0

      restartModel :: Int -> StdGen -> (Model f s, Double)
      restartModel restart =
          learnToRank miniBatchParams
                      (defaultConvergence info' convThreshold convMaxIter convDropIter)
                      evalCutoff trainData' fspace metric
        where
          info' = info <> " restart " <> show restart
      modelsWithTrainScore :: [(Model f s,Double)]
      modelsWithTrainScore = zipWith restartModel [0..] rngSeeds
     in take nRestarts modelsWithTrainScore


discardUntrainable :: TrainData f s q d -> TrainData f s q d
discardUntrainable evalData =
    M.filter hasPosAndNeg  evalData
  where
    hasPosAndNeg list =
        let hasPos = any (\(_,_,r) -> r == Relevant) list
            hasNeg = any (\(_,_,r) -> r /= Relevant) list
        in hasPos && hasNeg

bestRankingPerFold :: forall f s q d. ()
                   => BestFoldResults f s q d
                   -> Folds (M.Map q (Ranking SimplIR.LearningToRank.Score (d, Rel)))
bestRankingPerFold bestPerFold' =
    fmap (\(testData, ~(model, _trainScore))  ->  rerankRankings' model testData) bestPerFold'



data TrainedResult f s q d
    = TrainedResult
        { model :: Maybe (Model f s)
        , ranking :: M.Map q (Ranking Score (d, Rel))
        , testData :: M.Map q [(d, FeatureVec f s Double, Rel)]
        , modelDesc :: String
        , foldNo :: Maybe Integer
        , restartNo :: Maybe Integer
        , crossValidated :: Maybe Bool
        , trainMap :: Maybe Double
        , testMap :: Maybe Double
        , metric :: ScoringMetric IsRelevant q
        }

dumpKFoldModelsAndRankings
    :: forall f s q d. (Ord f, ToJSONKey f,  NFData q, Ord q, Show q, Show d, Render q, Render d)
    => Folds ( M.Map q [(d, FeatureVec f s Double, Rel)]
             , Restarts (Model f s, Double)
             )
       -- ^ test data and model restarts for each fold
    -> ScoringMetric IsRelevant q
    -> Folds (Restarts (TrainedResult f s q d))
dumpKFoldModelsAndRankings foldRestartResults metric =
    Folds
    [ Restarts
      [ TrainedResult { model = Just model
                      , ranking = ranking
                      , testData = testData
                      , modelDesc = modelDesc
                      , foldNo = Just $ fromIntegral foldNo
                      , restartNo = Just $ fromIntegral restartNo
                      , crossValidated = Just False
                      , trainMap = Just trainScore
                      , testMap = Nothing
                      , metric = metric
                      }
        | (RestartIdx restartNo, ~(model, trainScore)) <- toList $ indexedRestarts restartModels
        , let ranking = rerankRankings' model testData
        , let modelDesc = "fold-"<> show foldNo <> "-restart-"<> show restartNo
        ]
    | (FoldIdx foldNo, ~(testData, restartModels)) <- toList $ indexedFolds foldRestartResults
    ]



dumpFullModelsAndRankings
    :: forall f ph q d. (Ord f, ToJSONKey f,  NFData q, Ord q, Show q, Show d, Render q, Render d)
    => M.Map q [(d, FeatureVec f ph Double, Rel)]
    -> (Model f ph, Double)
    -> ScoringMetric IsRelevant q
    -> [TrainedResult f ph q d]
dumpFullModelsAndRankings trainData (model, trainScore) metric  =
    let modelDesc = "train"
        trainRanking = rerankRankings' model trainData
    in  [TrainedResult {model = Just model 
                    , ranking = trainRanking
                    , testData = M.empty
                    , modelDesc = modelDesc
                    , foldNo = Nothing
                    , restartNo = Nothing
                    , crossValidated = Just False
                    , testMap = Nothing
                    , trainMap = Nothing
                    , metric = metric
                    }  
        ]


-- storeModelAndRanking ::  forall f s q d. (Ord f, ToJSONKey f,  NFData q, Ord q, Show q, Show d, Render q, Render d)  
--     => FilePath
--     -> FilePath
--     -> (Maybe Integer -> Maybe [q] -> ModelEnvelope f s)
--     -> TrainedResult f s q d -> IO()
-- storeModelAndRanking outputFilePrefix experimentName modelEnvelope TrainedResult{..} = do
--     storeRankingData outputFilePrefix ranking metric modelDesc
--     when (isJust model) $
--             storeModelData outputFilePrefix experimentName (fromJust model) (Data.Maybe.fromMaybe 0.0 trainMap) modelDesc (modelEnvelope foldNo (Just $  M.keys testData))




-- l2rRankingToRankEntries :: forall q d. (Render q, Render d)
--                         => SimplirRun.MethodName
--                         -> M.Map q (Ranking SimplIR.LearningToRank.Score (d, Rel))
--                         -> [SimplirRun.RankingEntry]
-- l2rRankingToRankEntries methodName rankings =
--   [ SimplirRun.RankingEntry { queryId = render query
--                              , documentName = render doc
--                              , documentRank = rank
--                              , documentScore = rankScore
--                              , methodName = methodName
--                              }
--   | (query, ranking) <- M.toList rankings
--   , ((rankScore, (doc, _rel)), rank) <- Ranking.toSortedList ranking `zip` [1..]
--   ]


-- -- Train model on all data
-- storeModelData :: forall f ph d q. (ToJSONKey f, Ord f)
--                => FilePath
--                -> FilePath
--                -> Model f ph
--                -> Double
--                -> [Char]
--                -> ModelEnvelope f ph
--                -> IO ()
-- storeModelData outputFilePrefix experimentName model trainScore modelDesc modelEnvelope = do
--   putStrLn $ "Model "++modelDesc++ " train metric "++ (show trainScore) ++ " MAP."
--   let modelFile' = outputFilePrefix++experimentName++"-model-"++modelDesc++".json"
--   BSL.writeFile modelFile' $ Aeson.encode $ modelEnvelope model
--   putStrLn $ "Written model "++modelDesc++ " to file "++ (show modelFile') ++ " ."


-- loadOldModelData :: (Show f, FromJSONKey f, Ord f)
--                => FilePath
--                -> IO (SomeModel f)
-- loadOldModelData modelFile  = do
--   modelOpt <- Aeson.eitherDecode    <$> BSL.readFile modelFile 
--   return $
--     case modelOpt of
--       Left msg -> error $ "Issue deserializing model file "<> modelFile<> ": "<> msg
--       Right model -> model






-- storeRankingData ::  forall q d . (Show q, Show d, Render q, Render d) 
--                 => FilePath
--                -> M.Map  q (Ranking Double (d, IsRelevant))
--                -> ScoringMetric IsRelevant q
--                -> String
--                -> IO ()
-- storeRankingData outputFilePrefix ranking metric modelDesc = do
--   putStrLn $ "Model "++modelDesc++" test metric "++ show (metric ranking) ++ " MAP."
--   let runFile' = outputFilePrefix++"-run-"++modelDesc++".run"
--   SimplirRun.writeRunFile (runFile')
--        $ l2rRankingToRankEntries (T.pack $ "l2r "++modelDesc)
--        $ ranking

-- -- todo avoid duplicateion with storeRankingData
-- storeRankingDataNoMetric ::   forall q d . (Show q, Show d, Render q, Render d)
--                           =>  FilePath
--                          -> M.Map q (Ranking Double (d, Rel))
--                          -> String
--                          -> IO ()
-- storeRankingDataNoMetric outputFilePrefix ranking modelDesc = do
--   putStrLn $ "Model "++modelDesc++" .. no metric.."
--   let runFile' = outputFilePrefix++"-run-"++modelDesc++".run"
--   SimplirRun.writeRunFile (runFile')
--        $ l2rRankingToRankEntries (T.pack $ "l2r "++modelDesc)
--        $ ranking

