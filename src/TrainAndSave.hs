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

import Data.Function
import Data.Bifunctor
import System.Random
import Control.Parallel.Strategies
import Control.Concurrent.Map
import Control.DeepSeq

import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import SimplIR.FeatureSpace (FeatureSpace, FeatureVec)

import qualified SimplIR.Format.QRel as QRel
import qualified SimplIR.Ranking as Ranking
import SimplIR.TrainUtils

import Debug.Trace as Debug

import qualified SimplIR.Format.TrecRunFile as SimplirRun
import GHC.Generics (Generic)


type Q = SimplirRun.QueryId
type DocId = SimplirRun.DocumentName
type Rel = IsRelevant
type TrainData f s = M.Map Q [(DocId, FeatureVec f s Double, Rel)]

type FoldRestartResults f s = Folds (M.Map Q [(DocId, FeatureVec f s Double, Rel)],
                                    [(Model f s, Double)])
type BestFoldResults f s = Folds (M.Map Q [(DocId, FeatureVec f s Double, Rel)], (Model f s, Double))



-- data RankLipsModel f s = RankLipsModel { --trainedModel :: Model f s
--                                        , minibatchParamsOpt :: Maybe MiniBatchParams
--                                        , evalCutoffOpt :: Maybe EvalCutoff
--                                        , convergenceDiagParameters :: Maybe ConvergenceDiagParams
--                                        , useZscore :: Maybe Bool
--                                         -- , useCv :: Maybe Bool
--                                        , experimentName :: Maybe String
--                                        }


-- RankLipsModel {
--   trainedModel :: Model f s
--   , useCv :: Maybe Bool

-- }                  

data RankLipsModel f s = RankLipsModel { trainedModel :: Model f s
                                       , minibatchParamsOpt :: Maybe MiniBatchParams
                                       , convergenceDiagParameters :: Maybe ConvergenceDiagParams
                                       , useZscore :: Maybe Bool
                                       , cvFold :: Maybe Integer
                                       , heldoutQueries :: Maybe [SimplirRun.QueryId]
                                       , experimentName :: Maybe String
                                       , rankLipsVersion :: Maybe String
                                       }
  

defaultRankLipsModel :: Model f s  -> RankLipsModel f s
defaultRankLipsModel model = RankLipsModel model Nothing Nothing Nothing Nothing Nothing Nothing Nothing


data RankLipsMetaField = RankLipsMiniBatch MiniBatchParams 
                       | RankLipsConvergenceDiagParams ConvergenceDiagParams
                       | RankLipsUseZScore Bool
                       | RankLipsCVFold Integer
                       | RankLipsHeldoutQueries [SimplirRun.QueryId]
                       | RankLipsIsFullTrain
                       | RankLipsExperimentName String
                       | RankLipsVersion String
  deriving (Show, Generic, ToJSON, FromJSON)

data RankLipsModelSerialized f = RankLipsModelSerialized { rankLipsTrainedModel :: SomeModel f
                                                          , rankLipsMetaData :: [RankLipsMetaField]
                                                          }
  deriving (Generic, ToJSON, FromJSON)

data SomeRankLipsModel f where 
    SomeRankLipsModel :: RankLipsModel f s -> SomeRankLipsModel f



loadRankLipsModel :: (FromJSONKey f, Ord f, Show f) => FilePath -> IO (RankLipsModelSerialized f)
loadRankLipsModel modelFile = do
  modelOrErr <- Aeson.eitherDecode  <$> BSL.readFile modelFile 
  return $
    case modelOrErr of
      Left msg -> error $ "Issue deserializing model file "<> modelFile<> ": "<> msg
      Right model -> model



type ModelEnvelope f s = Model f s -> RankLipsModelSerialized f


-- --------------------------------------------

trainMe :: forall f s. (Ord f, ToJSONKey f, Show f)
        => Bool
        -> MiniBatchParams
        -> ConvergenceDiagParams
        -> StdGen
        -> TrainData f s
        -> FeatureSpace f s
        -> ScoringMetric IsRelevant Q
        -> FilePath
        -> FilePath
        -> (Maybe Integer -> Maybe [SimplirRun.QueryId] -> Model f s -> RankLipsModelSerialized f)
        -> IO ()
trainMe includeCv miniBatchParams convDiagParams gen0 trainData fspace metric outputFilePrefix experimentName modelEnvelope = do
          -- train me!
          let nRestarts = convergenceRestarts convDiagParams
              nFolds = convergenceFolds convDiagParams


          -- folded CV
                                -- todo load external folds
              !folds = force $ mkSequentialFolds nFolds (M.keys trainData)
          putStrLn "made folds"
          putStrLn $ unwords [ show $ length f | f <- getFolds folds ]

          let trainFun :: FoldIdx -> TrainData f s -> [(Model f s, Double)]
              trainFun foldIdx =
                  take nRestarts . trainWithRestarts miniBatchParams convDiagParams gen0 metric infoStr fspace
                where
                  infoStr = show foldIdx

              foldRestartResults :: Folds (M.Map  Q [(DocId, FeatureVec f s Double, Rel)], [(Model f s, Double)])
              foldRestartResults = trainKFolds trainFun trainData folds

              strat :: Strategy (Folds (a, [(Model f s, Double)]))
              strat = parTraversable (evalTuple2 r0 (parTraversable rdeepseq))
          
          putStrLn "full Train"
          -- full train
          let fullRestarts = withStrategy (parTraversable rdeepseq)
                             $ take nRestarts $ trainWithRestarts miniBatchParams convDiagParams gen0 metric "full" fspace trainData
              (model, trainScore) = Debug.trace ("full Train - best Model") 
                                  $    bestModel $  fullRestarts
              fullActions = Debug.trace ("full Train - dump Model")
                          $ dumpFullModelsAndRankings trainData (model, trainScore) metric outputFilePrefix experimentName modelEnvelope


          putStrLn "CV Train"
          if includeCv
            then do
              foldRestartResults' <- withStrategyIO strat foldRestartResults
              let cvActions = dumpKFoldModelsAndRankings foldRestartResults' metric outputFilePrefix experimentName modelEnvelope
              putStrLn "concurrently: CV Train"
              mapConcurrentlyL_ 24 id $ fullActions ++ cvActions
            else
              mapConcurrentlyL_ 24 id $ fullActions
          putStrLn "dumped all models and rankings"


data ConvergenceDiagParams = ConvergenceDiagParams { convergenceThreshold :: Double
                                                  , convergenceMaxIter :: Int
                                                  , convergenceDropInitIter :: Int
                                                  , convergenceEvalCutoff :: EvalCutoff
                                                  , convergenceRestarts :: Int
                                                  , convergenceFolds :: Int
                                                  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)


trainWithRestarts
    :: forall f s. (ToJSONKey f, Show f)
    => MiniBatchParams
    -> ConvergenceDiagParams
    -> StdGen
    -> ScoringMetric IsRelevant Q
    -> String
    -> FeatureSpace f s
    -> TrainData f s
    -> [(Model f s, Double)]
       -- ^ an infinite list of restarts
trainWithRestarts miniBatchParams (ConvergenceDiagParams convThreshold convMaxIter convDropIter evalCutoff _numRestarts _numFolds) gen0 metric info fspace trainData =
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
     in modelsWithTrainScore


discardUntrainable :: TrainData f s -> TrainData f s
discardUntrainable evalData =
    M.filter hasPosAndNeg  evalData
  where
    hasPosAndNeg list =
        let hasPos = any (\(_,_,r) -> r == Relevant) list
            hasNeg = any (\(_,_,r) -> r /= Relevant) list
        in hasPos && hasNeg


bestPerFold :: FoldRestartResults f s -> BestFoldResults f s
bestPerFold = fmap (second bestModel)

bestModel ::  [(Model f s, Double)] -> (Model f s, Double)
bestModel = maximumBy (compare `on` snd)


bestRankingPerFold :: forall f  s. ()
                   => BestFoldResults f s
                   -> Folds (M.Map Q (Ranking SimplIR.LearningToRank.Score (DocId, Rel)))
bestRankingPerFold bestPerFold' =
    fmap (\(testData, ~(model, _trainScore))  ->  rerankRankings' model testData) bestPerFold'



dumpKFoldModelsAndRankings
    :: forall f s. (Ord f, ToJSONKey f)
    => FoldRestartResults f s
    -> ScoringMetric IsRelevant Q
    -> FilePath
    -> FilePath
    -> (Maybe Integer -> Maybe [SimplirRun.QueryId] -> ModelEnvelope f s)
    -> [IO ()]
dumpKFoldModelsAndRankings foldRestartResults metric outputFilePrefix experimentName modelEnvelope =
    let bestPerFold' :: Folds (M.Map Q [(DocId, FeatureVec f s Double, Rel)], (Model f s, Double))
        bestPerFold' = bestPerFold foldRestartResults

        bestRankingPerFold' :: Folds (M.Map Q (Ranking SimplIR.LearningToRank.Score (DocId, Rel)))
        bestRankingPerFold' = bestRankingPerFold bestPerFold'

        testRanking ::   M.Map Q (Ranking SimplIR.LearningToRank.Score (DocId, Rel))
        testRanking = fold bestRankingPerFold'

        _testScore = metric testRanking

--         dumpAll = [ do storeRankingData outputFilePrefix ranking metric modelDesc
--                        storeModelData outputFilePrefix experimentName model trainScore modelDesc
--                   | (foldNo, ~(testData, restartModels))  <- zip [0..] $ toList foldRestartResults
--                   , (restartNo, ~(model, trainScore)) <- zip [0..] restartModels
--                   , let ranking = rerankRankings' model testData
--                   , let modelDesc = "fold-"<> show foldNo <> "-restart-"<> show restartNo
--                   ]

        dumpAll = []

        dumpBest =
            [ do storeRankingData outputFilePrefix ranking metric modelDesc
                 storeModelData outputFilePrefix experimentName model trainScore modelDesc (modelEnvelope (Just foldNo) (Just $  M.keys testData))
            | (foldNo, (testData,  ~(model, trainScore)))  <- zip [0 :: Integer ..]
                                                              $ toList bestPerFold'
            , let ranking = rerankRankings' model testData
            , let modelDesc = "fold-"<> show foldNo <> "-best"
            ]

        dumpKfoldTestRanking = storeRankingData outputFilePrefix testRanking metric modelDesc
          where modelDesc = "test"

    in dumpAll ++ dumpBest ++ [dumpKfoldTestRanking]



dumpFullModelsAndRankings
    :: forall f ph. (Ord f, ToJSONKey f)
    => M.Map Q [(DocId, FeatureVec f ph Double, Rel)]
    -> (Model f ph, Double)
    -> ScoringMetric IsRelevant SimplirRun.QueryId
    -> FilePath
    -> FilePath
    -> (Maybe Integer -> Maybe [SimplirRun.QueryId] -> ModelEnvelope f ph)
    -> [IO()]
dumpFullModelsAndRankings trainData (model, trainScore) metric outputFilePrefix experimentName modelEnvelope =
    let modelDesc = "train"
        trainRanking = rerankRankings' model trainData
    in [ storeRankingData outputFilePrefix trainRanking metric modelDesc
       , storeModelData outputFilePrefix experimentName model trainScore modelDesc (modelEnvelope Nothing Nothing)
       ]




l2rRankingToRankEntries :: SimplirRun.MethodName
                        -> M.Map Q (Ranking SimplIR.LearningToRank.Score (SimplirRun.DocumentName, Rel))
                        -> [SimplirRun.RankingEntry]
l2rRankingToRankEntries methodName rankings =
  [ SimplirRun.RankingEntry { queryId = query
                             , documentName = doc
                             , documentRank = rank
                             , documentScore = rankScore
                             , methodName = methodName
                             }
  | (query, ranking) <- M.toList rankings
  , ((rankScore, (doc, _rel)), rank) <- Ranking.toSortedList ranking `zip` [1..]
  ]


-- Train model on all data
storeModelData :: (ToJSONKey f, Ord f)
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






storeRankingData ::  FilePath
               -> M.Map  Q (Ranking Double (QRel.DocumentName, IsRelevant))
               -> ScoringMetric IsRelevant Q
               -> String
               -> IO ()
storeRankingData outputFilePrefix ranking metric modelDesc = do
  putStrLn $ "Model "++modelDesc++" test metric "++ show (metric ranking) ++ " MAP."
  let runFile' = outputFilePrefix++"-run-"++modelDesc++".run"
  SimplirRun.writeRunFile (runFile')
       $ l2rRankingToRankEntries (T.pack $ "l2r "++modelDesc)
       $ ranking

-- todo avoid duplicateion with storeRankingData
storeRankingDataNoMetric :: FilePath
                         -> M.Map SimplirRun.QueryId (Ranking Double (QRel.DocumentName, Rel))
                         -> String
                         -> IO ()
storeRankingDataNoMetric outputFilePrefix ranking modelDesc = do
  putStrLn $ "Model "++modelDesc++" .. no metric.."
  let runFile' = outputFilePrefix++"-run-"++modelDesc++".run"
  SimplirRun.writeRunFile (runFile')
       $ l2rRankingToRankEntries (T.pack $ "l2r "++modelDesc)
       $ ranking

