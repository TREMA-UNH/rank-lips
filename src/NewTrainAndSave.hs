
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

module NewTrainAndSave where

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

import TrainAndSave


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






-- trainAndStore :: forall f s q d. (Ord f, ToJSONKey f, Show f, NFData q, Ord q, Show q, Show d, Render q, Render d)
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
-- trainAndStore includeCv miniBatchParams convDiagParams gen0 trainData fspace metric outputFilePrefix experimentName modelEnvelope = do
--           putStrLn "made folds"
--           putStrLn $ unwords [ show $ length f | f <- getFolds folds ]

-- TODO: plug IO bits from trainMe and move to trainAndStore


trainMe :: forall f s q d. (Ord f, ToJSONKey f, Show f, NFData q, Ord q, Show q, Show d, Render q, Render d)
        => Bool
        -> MiniBatchParams
        -> ConvergenceDiagParams
        -> StdGen
        -> TrainData f s q d
        -> FeatureSpace f s
        -> ScoringMetric IsRelevant q
        -> FilePath
        -> FilePath
        -> (Maybe Integer -> Maybe [q] -> Model f s -> RankLipsModelSerialized f)
        -> IO ()
        -- -> _
trainMe includeCv miniBatchParams convDiagParams gen0 trainData fspace metric outputFilePrefix experimentName modelEnvelope = do
          -- train me!
          let nRestarts = convergenceRestarts convDiagParams
              nFolds = convergenceFolds convDiagParams


          -- folded CV
                                -- todo load external folds
              !folds = force $ mkSequentialFolds nFolds (M.keys trainData)
          putStrLn "made folds"
          putStrLn $ unwords [ show $ length f | f <- getFolds folds ]

          let trainFun :: FoldIdx -> TrainData f s q d -> [(Model f s, Double)]
              trainFun foldIdx =
                  trainWithRestarts nRestarts miniBatchParams convDiagParams gen0 metric infoStr fspace
                where
                  infoStr = show foldIdx

              foldRestartResults :: Folds (M.Map  q [(d, FeatureVec f s Double, Rel)], [(Model f s, Double)])
              foldRestartResults = trainKFolds trainFun trainData folds

              strat :: Strategy (Folds (a, [(Model f s, Double)]))
              strat = parTraversable (evalTuple2 r0 (parTraversable rdeepseq))
          
          putStrLn "full Train"
          -- full train
          let fullRestarts = withStrategy (parTraversable rdeepseq)
                             $ trainWithRestarts nRestarts miniBatchParams convDiagParams gen0 metric "full" fspace trainData
              (model, trainScore) =  bestModel $  fullRestarts
              fullActions = fmap (storeModelAndRanking outputFilePrefix experimentName modelEnvelope ) $ dumpFullModelsAndRankings trainData (model, trainScore) metric


          putStrLn "CV Train"
          if includeCv
            then do
              foldRestartResults' <- withStrategyIO strat foldRestartResults
              let cvActions = fmap (storeModelAndRanking outputFilePrefix experimentName modelEnvelope) $  dumpKFoldModelsAndRankings foldRestartResults' metric 
              putStrLn "concurrently: CV Train"
              mapConcurrentlyL_ 24 id $ fullActions ++ cvActions
            else
              mapConcurrentlyL_ 24 id $ fullActions
          putStrLn "dumped all models and rankings"