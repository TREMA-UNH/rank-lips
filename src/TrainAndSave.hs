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
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.List
import Data.Foldable as Foldable

import System.Random
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Ord

import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import SimplIR.FeatureSpace (FeatureSpace, FeatureVec)

import qualified SimplIR.Ranking as Ranking
import SimplIR.TrainUtils

import Debug.Trace as Debug

import qualified SimplIR.Format.TrecRunFile as SimplirRun

import RankLipsTypes
import Data.Maybe
import GHC.Base (when)

type ModelEnvelope f s = Model f s -> RankLipsModelSerialized f


-- --------------------------------------------



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
    -> Restarts (Model f s, Double)
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
     in Restarts $ take nRestarts modelsWithTrainScore


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
                      , crossValidated = Nothing
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
    :: forall f s q d. (Ord f, ToJSONKey f,  NFData q, Ord q, Show q, Show d, Render q, Render d)
    =>  M.Map q [(d, FeatureVec f s Double, Rel)]
    ->  Restarts (Model f s, Double)
    -> ScoringMetric IsRelevant q
    -> Restarts (TrainedResult f s q d)
dumpFullModelsAndRankings trainData restartModels metric =
    Restarts
      [ TrainedResult { model = Just model
                      , ranking = ranking
                      , testData = trainData
                      , modelDesc = modelDesc
                      , foldNo = Nothing
                      , restartNo = Just $ fromIntegral restartNo
                      , crossValidated = Just False
                      , trainMap = Just trainScore
                      , testMap = Nothing
                      , metric = metric
                      }
        | (RestartIdx restartNo, ~(model, trainScore)) <- toList $ indexedRestarts restartModels
        , let ranking = rerankRankings' model trainData
        , let modelDesc = "train-restart-"<> show restartNo
        ]
    


storeModelAndRanking ::  forall f s q d. (Ord f, ToJSONKey f,  NFData q, Ord q, Show q, Show d, Render q, Render d)  
    => FilePath
    -> FilePath
    -> (Maybe Integer -> Maybe [q] -> ModelEnvelope f s)
    -> TrainedResult f s q d -> IO()
storeModelAndRanking outputFilePrefix experimentName modelEnvelope TrainedResult{..} = do
    storeRankingData outputFilePrefix ranking metric modelDesc
    when (isJust model) $
            storeModelData outputFilePrefix experimentName (fromJust model) (Data.Maybe.fromMaybe 0.0 trainMap) modelDesc (modelEnvelope foldNo (Just $  M.keys testData))




l2rRankingToRankEntries :: forall q d. (Render q, Render d)
                        => SimplirRun.MethodName
                        -> M.Map q (Ranking SimplIR.LearningToRank.Score (d, Rel))
                        -> [SimplirRun.RankingEntry]
l2rRankingToRankEntries methodName rankings =
  [ SimplirRun.RankingEntry { queryId = render query
                             , documentName = render doc
                             , documentRank = rank
                             , documentScore = rankScore
                             , methodName = methodName
                             }
  | (query, ranking) <- M.toList rankings
  , ((rankScore, (doc, _rel)), rank) <- Ranking.toSortedList ranking `zip` [1..]
  ]


-- Train model on all data
storeModelData :: forall f ph d q. (ToJSONKey f, Ord f)
               => FilePath
               -> FilePath
               -> Model f ph
               -> Double
               -> [Char]
               -> ModelEnvelope f ph
               -> IO ()
storeModelData outputFilePrefix experimentName model trainScore modelDesc modelEnvelope = do
  putStrLn $ "Model "++modelDesc++ " train metric "++ (show trainScore) ++ " MAP."
  let modelFile' = outputFilePrefix++experimentName++"-model-"++modelDesc++".json"
  BSL.writeFile modelFile' $ Aeson.encode $ modelEnvelope model
  putStrLn $ "Written model "++modelDesc++ " to file "++ (show modelFile') ++ " ."


loadOldModelData :: (Show f, FromJSONKey f, Ord f)
               => FilePath
               -> IO (SomeModel f)
loadOldModelData modelFile  = do
  modelOpt <- Aeson.eitherDecode    <$> BSL.readFile modelFile 
  return $
    case modelOpt of
      Left msg -> error $ "Issue deserializing model file "<> modelFile<> ": "<> msg
      Right model -> model

storeRankingData ::  forall q d . (Show q, Show d, Render q, Render d) 
                => FilePath
               -> M.Map  q (Ranking Double (d, IsRelevant))
               -> ScoringMetric IsRelevant q
               -> String
               -> IO ()
storeRankingData outputFilePrefix ranking metric modelDesc = do
  putStrLn $ "Model "++modelDesc++" test metric "++ show (metric ranking) ++ " MAP."
  let runFile' = outputFilePrefix++"-run-"++modelDesc++".run"
  SimplirRun.writeRunFile (runFile')
       $ l2rRankingToRankEntries (T.pack $ "l2r "++modelDesc)
       $ ranking

-- todo avoid duplicateion with storeRankingData
storeRankingDataNoMetric ::   forall q d . (Show q, Show d, Render q, Render d)
                          =>  FilePath
                         -> M.Map q (Ranking Double (d, Rel))
                         -> String
                         -> IO ()
storeRankingDataNoMetric outputFilePrefix ranking modelDesc = do
  putStrLn $ "Model "++modelDesc++" .. no metric.."
  let runFile' = outputFilePrefix++"-run-"++modelDesc++".run"
  SimplirRun.writeRunFile (runFile')
       $ l2rRankingToRankEntries (T.pack $ "l2r "++modelDesc)
       $ ranking

mapIOCvFull :: (a -> IO b) -> (Folds a, a) -> IO (( Folds b), b) 
mapIOCvFull f (cvModels, fullModels) = do
    (,) <$> mapM f cvModels <*> f fullModels

mapCvFull :: (a -> b) -> (Folds a, a) -> (( Folds b), b) 
mapCvFull f (cvModels, fullModels) = do
    ( fmap f cvModels, f fullModels)


bestRestartBy :: (a -> a -> Ordering) -> Restarts a -> a
bestRestartBy f = maximumBy f

bestRestart :: Restarts (TrainedResult f s q d) -> TrainedResult f s q d
bestRestart models =
    let trained = bestRestartBy (comparing trainMap) models
    in trained { modelDesc = case (foldNo trained) of 
                                      Just no -> "fold-"<> show (no) <> "-best"
                                      Nothing -> "train"
               }


computeTestRanking :: forall f s q d. (Ord q)
                   => Folds (TrainedResult f s q d) -> TrainedResult f s q d
computeTestRanking foldResults =
    TrainedResult { model = Nothing
                  , ranking = testRanking
                  , testData = M.empty
                  , modelDesc = "test"
                  , foldNo = Nothing
                  , restartNo = Nothing
                  , crossValidated = Just True
                  , trainMap = Nothing
                  , testMap = Just testScore
                  , metric = metric'
                  }
  where
    testRanking :: M.Map q (Ranking Score (d, Rel))
    testRanking = Foldable.foldMap (\TrainedResult {testData = testData, model = Just model} -> rerankRankings' model testData) foldResults

    metric' :: ScoringMetric IsRelevant q
    metric' = metric $ head $ toList foldResults
    testScore = metric' testRanking

trainMe :: forall f s q d. (Ord f, ToJSONKey f, Show f, NFData q, Ord q, Show q, Show d, Render q, Render d)
        => MiniBatchParams
        -> ConvergenceDiagParams
        -> StdGen
        -> TrainData f s q d
        -> FeatureSpace f s
        -> ScoringMetric IsRelevant q
        -> (Restarts (TrainedResult f s q d), Folds (Restarts (TrainedResult f s q d) ))
trainMe miniBatchParams convDiagParams gen0 trainData fspace metric =
  -- train me!
  let nRestarts = convergenceRestarts convDiagParams
      nFolds = convergenceFolds convDiagParams

      -- folded CV
      -- todo load external folds
      !folds = force $ mkSequentialFolds nFolds (M.keys trainData)
      trainFun :: FoldIdx -> TrainData f s q d -> Restarts (Model f s, Double)
      trainFun foldIdx =
          trainWithRestarts nRestarts miniBatchParams convDiagParams gen0 metric infoStr fspace
        where
          infoStr = show foldIdx

      foldRestartResults :: Folds (M.Map q [(d, FeatureVec f s Double, Rel)], Restarts (Model f s, Double))
      foldRestartResults = trainKFolds trainFun trainData folds
  
      -- full train
      fullRestarts :: Restarts (Model f s, Double)
      fullRestarts = trainWithRestarts nRestarts miniBatchParams convDiagParams gen0 metric "train" fspace trainData
    --   bestFull = bestRestart (comparing snd) fullRestarts

      fullActions :: Restarts (TrainedResult f s q d)
      fullActions = dumpFullModelsAndRankings trainData fullRestarts metric

      folds' :: Folds (Restarts (TrainedResult f s q d))
      folds' = dumpKFoldModelsAndRankings foldRestartResults metric 
  in  (fullActions, folds')

