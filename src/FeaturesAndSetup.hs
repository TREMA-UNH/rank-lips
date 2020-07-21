{-# LANGUAGE DerivingStrategies #-}
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
{-# LANGUAGE TemplateHaskell #-}


module FeaturesAndSetup where

import Control.DeepSeq hiding (rwhnf)
import Control.Parallel.Strategies
import Data.Semigroup hiding (All, Any, option)
import System.Random
import System.FilePath

import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe

import qualified SimplIR.Format.TrecRunFile as SimplirRun
import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (FeatureVec)
import SimplIR.FeatureSpace.Normalise

import qualified SimplIR.Format.QRel as QRel

import TrainAndSave
import RankLipsTypes
import Data.Bifunctor (Bifunctor(second))
import qualified Data.Aeson as Aeson

import SimplIR.Format.JsonRunQrels
import QrelInfo
import RankLipsFeatureUtils
import GHC.Stack (HasCallStack)
import Control.Concurrent.Map (mapConcurrentlyL)



createModelEnvelope :: (Show q)
                    => (Model f ph -> Model f ph)
                    -> Maybe String
                    -> Maybe MiniBatchParams
                    -> Maybe ConvergenceDiagParams
                    -> Maybe Bool
                    -> Maybe Bool
                    -> Maybe String
                    -> Maybe DefaultFeatureParams
                    -> (Maybe Integer -> Maybe [q] -> ModelEnvelope f ph)
createModelEnvelope modelConv experimentName minibatchParamsOpt convergenceDiagParameters useZscore saveHeldoutQueriesInModel version defaultFeatureParams =
    (\cvFold heldOutQueries someModel' -> 
        let trainedModel = modelConv someModel'
            rankLipsModel = RankLipsModel { rankLipsVersion = version
                                          , heldoutQueries = if (fromMaybe False saveHeldoutQueriesInModel) 
                                                                 then fmap (fmap show) heldOutQueries
                                                                 else Nothing
                                          , .. }
        in serializeRankLipsModel rankLipsModel
    )


doPredict :: forall ph q d  . (Ord q, Ord d, Show q, Show d, Render q, Render d, Aeson.FromJSON q, Aeson.FromJSON d)
             => (SimplirRun.QueryId -> q) -> (SimplirRun.DocumentName -> d) 
            -> FeatureParams
            -> FilePath 
            -> Maybe DefaultFeatureParams
            -> Model Feat ph
            -> Maybe FilePath 
            -> IO () 
doPredict convQ convD featureParams@FeatureParams{..} outputFilePrefix defaultFeatureParamsOpt model qrelFileOpt  = do
    let fspace = modelFeatures model
        defaultFeatureVec = createDefaultFeatureVec fspace defaultFeatureParamsOpt
        FeatureSet {featureNames=_featureNames, produceFeatures=produceFeatures}
           = featureSet featureParams

    runFiles <- if featuresRunFormat == TrecEvalRunFormat
                    then loadRunFiles convQ convD featureRunsDirectory features
                    else loadJsonLRunFiles featuresRunFormat featureRunsDirectory features

    putStrLn $ " loadRunFiles " <> (unwords $ fmap fst runFiles)


    QrelInfo{..} <- case qrelFileOpt of
                        Just qrelFile -> do
                            loadQrelInfo <$> readTrecEvalQrelFile convQ convD qrelFile
                        Nothing -> return $ noQrelInfo


    let featureDataMap = runFilesToFeatureVectorsMap fspace defaultFeatureVec produceFeatures runFiles
        featureDataList = fmap M.toList featureDataMap

        allDataList :: M.Map q [( d, FeatureVec Feat ph Double, QRel.IsRelevant)]
        allDataList = augmentWithQrelsList_ (lookupQrel QRel.NotRelevant) featureDataList

        ranking = withStrategy (parTraversable rseq) 
                $ rerankRankings' model allDataList    

    case qrelFileOpt of
        Just _ -> storeRankingData outputFilePrefix ranking metric "predict"
        Nothing -> storeRankingDataNoMetric outputFilePrefix ranking "predict"


doTrain :: forall q d . (Ord q, Ord d, Show q, Show d, NFData q, NFData d, Aeson.FromJSON q, Aeson.FromJSON d, Render q, Render d)
            => (SimplirRun.QueryId -> q) -> (SimplirRun.DocumentName -> d) 
            -> FeatureParams
            -> FilePath 
            -> FilePath 
            -> FilePath 
            -> MiniBatchParams
            -> Bool
            -> Bool
            -> Bool
            -> ConvergenceDiagParams
            -> Maybe DefaultFeatureParams
            -> String
            -> IO ()
doTrain convQ convD featureParams@FeatureParams{..} outputFilePrefix experimentName qrelFile 
        miniBatchParams includeCv useZScore saveHeldoutQueriesInModel convergenceParams 
        defaultFeatureParamsOpt rankLipsVersion = do
    let FeatureSet {featureNames=featureNames,  produceFeatures=produceFeatures}
         = featureSet featureParams


    F.SomeFeatureSpace (fspace:: F.FeatureSpace Feat ph) <- pure $ F.mkFeatureSpace featureNames

    runFiles <- if  featuresRunFormat == TrecEvalRunFormat
                    then loadRunFiles convQ convD featureRunsDirectory features
                    else loadJsonLRunFiles featuresRunFormat featureRunsDirectory features
    putStrLn $ " loadRunFiles " <> (unwords $ fmap fst runFiles)

    QrelInfo{..} <- loadQrelInfo <$> readTrecEvalQrelFile convQ convD qrelFile

    let defaultFeatureVec =  createDefaultFeatureVec fspace defaultFeatureParamsOpt

        featureDataMap = runFilesToFeatureVectorsMap  fspace defaultFeatureVec produceFeatures runFiles
        featureDataList :: M.Map q [( d, (F.FeatureVec Feat ph Double))] 
        featureDataList = fmap M.toList featureDataMap

        (featureDataList', createModelEnvelope') =
            if useZScore
                then
                    let zNorm :: Normalisation Feat ph Double
                        zNorm = zNormalizer $ [ feat
                                            | (_, list )<- M.toList featureDataList
                                            , (_, feat ) <- list
                                            ]
                        featureDataListZscore :: M.Map q [( d, (F.FeatureVec Feat ph Double))] 
                        featureDataListZscore = fmap normDocs featureDataList
                          where normDocs list =
                                    [ (doc, (normFeatures zNorm) feat)
                                    | (doc, feat) <- list    
                                    ]

                        modelConv (Model (WeightVec weights)) = Model (WeightVec $ denormWeights zNorm weights)

                    in (featureDataListZscore, createModelEnvelope modelConv)

                else (featureDataList, createModelEnvelope id)

        allDataListRaw :: M.Map q [( d, FeatureVec Feat ph Double, Rel)]
        allDataListRaw = augmentWithQrelsList_ (lookupQrel QRel.NotRelevant) featureDataList'

        allDataList = allDataListRaw


        modelEnvelope = createModelEnvelope' (Just experimentName) (Just miniBatchParams) (Just convergenceParams) (Just useZScore) (Just saveHeldoutQueriesInModel) (Just rankLipsVersion) defaultFeatureParamsOpt

    train includeCv fspace allDataList qrelData miniBatchParams convergenceParams  outputFilePrefix modelEnvelope


train :: forall ph q d . (Ord q, Ord d, Show q, Show d, NFData q, NFData d, Render q, Render d, HasCallStack)
      =>  Bool
      -> F.FeatureSpace Feat ph
      -> TrainData Feat ph q d
      -> [QRel.Entry q d IsRelevant]
      -> MiniBatchParams
      -> ConvergenceDiagParams
      -> FilePath
      -> (Maybe Integer -> Maybe [q] -> Model Feat ph -> RankLipsModelSerialized Feat)
      -> IO()
train includeCv fspace allData qrel miniBatchParams convergenceDiagParams outputFilePrefix modelEnvelope =  do
    let metric :: ScoringMetric IsRelevant q
        !metric = meanAvgPrec (totalRelevantFromQRels qrel) Relevant
        totalElems = getSum . foldMap ( Sum . length ) $ allData
        totalPos = getSum . foldMap ( Sum . length . filter (\(_,_,rel) -> rel == Relevant)) $ allData

    putStrLn $ "Feature dimension: "++show (F.dimension $ F.featureSpace $ (\(_,a,_) -> a) $ head' $ snd $ M.elemAt 0 allData)
    putStrLn $ "Training model with (trainData) "++ show (M.size allData) ++
            " queries and "++ show totalElems ++" items total of which "++
            show totalPos ++" are positive."
    let displayTrainData :: Show f => TrainData f ph q d -> [String]
        displayTrainData trainData =
            [ show k ++ " " ++ show d ++ " " ++ show r ++ " -> "++ prettyFv
            | (k,list) <- M.toList trainData
            , (d,fvec, r) <- list
            , let prettyFv = unlines $ fmap show $ F.toList fvec
            ]

    putStrLn $ "Training Data = \n" ++ intercalate "\n" (take 10 $ displayTrainData $ force allData)
    gen0 <- newStdGen  -- needed by learning to rank
    -- trainAndStore 

    let experimentName = ""

    putStrLn "made folds"
    let (fullRestartResults, foldRestartResults) =
            trainMe miniBatchParams convergenceDiagParams gen0 allData fspace metric

    let savedTrainedResult model = do
            storeModelAndRanking outputFilePrefix experimentName modelEnvelope model
            return model

        strat :: Strategy [TrainedResult f s q d]
        strat = parBuffer 24 rseq
    
    (cvModels, fullModels) <- mapIOCvFull savedTrainedResult
                            $ withStrategy (parTuple2 (parTraversable rseq) rseq)
                            $ ( fmap bestRestart  (foldRestartResults)
                              , (bestRestart fullRestartResults)
                              )

    let testRanking = computeTestRanking  $ cvModels
    _ <- savedTrainedResult testRanking
    return ()



loadRunFiles :: forall q d 
             .  (SimplirRun.QueryId -> q) -> (SimplirRun.DocumentName -> d) 
             -> FilePath -> [FilePath] ->  IO [(FilePath, [SimplirRun.RankingEntry' q d])] 
loadRunFiles convQ convD prefix inputRuns = do
    fmap (second (map conv)) 
    <$> mapM (\fname -> (fname,)
    <$> SimplirRun.readRunFile (prefix </> fname)) inputRuns    
  where conv :: SimplirRun.RankingEntry -> SimplirRun.RankingEntry' q d    
        conv SimplirRun.RankingEntry{..} =
            SimplirRun.RankingEntry{ queryId= convQ queryId
                                    , documentName = convD documentName
                                    , .. }
loadJsonLRunFiles :: forall q d 
                  . (Aeson.FromJSON q, Aeson.FromJSON d)
                  => RunFormat -> FilePath -> [FilePath] ->  IO [(FilePath, [SimplirRun.RankingEntry' q d])] 
loadJsonLRunFiles JsonLRunFormat prefix inputRuns = do
    mapConcurrentlyL 20 ((\fname -> (fname,)
                            <$> readJsonLRunFile (prefix </> fname))
                        ) inputRuns    

loadJsonLRunFiles JsonLGzRunFormat prefix inputRuns = do
    mapConcurrentlyL 20 ( (\fname -> (fname,)
                        <$> readGzJsonLRunFile (prefix </> fname))
                        ) inputRuns    

loadJsonLRunFiles TrecEvalRunFormat prefix inputRuns = do
    fail $ "loadJsonLRunFiles TrecEvalRunFormat not supported."


augmentWithQrelsList_ ::  forall ph q d
                 . (q -> d -> Rel) 
                 -> M.Map q [( d, (F.FeatureVec Feat ph Double))] 
                 -> M.Map q [(d, F.FeatureVec Feat ph Double, Rel)]
augmentWithQrelsList_ lookupQrel qData =
    M.mapWithKey mapQueries qData
  where mapQueries :: q -> [(d, F.FeatureVec Feat ph Double)] -> [(d, F.FeatureVec Feat ph Double, Rel)]
        mapQueries query dMap =
            fmap mapDocs dMap
          where mapDocs :: (d,  F.FeatureVec Feat ph Double) -> (d,  F.FeatureVec Feat ph Double ,Rel)
                mapDocs (doc, feats) =
                    (doc, feats, lookupQrel query doc)


-- internFeatures :: F.FeatureSpace Feat ph -> [(Feat, d)] -> [(Feat, d)]
-- internFeatures fspace features =
--     fmap internF features
--   where 
--     internF (f,v) =  
--       let f' = F.internFeatureName fspace f
--       in case f' of
--             Just f'' -> (f'', v)
--             Nothing -> error $ "Trying to intern feature "<> show f <> ", but is not defined in featurespace."


runFilesToFeatureVectorsMap :: forall ph q d . (Ord q, Ord d)
                          => F.FeatureSpace Feat ph 
                          -> F.FeatureVec Feat ph Double
                          -> (FilePath -> SimplirRun.RankingEntry' q d -> [(Feat, Double)])
                          -> [(FilePath, [SimplirRun.RankingEntry' q d])] 
                          -> M.Map q (M.Map d (F.FeatureVec Feat ph Double))
runFilesToFeatureVectorsMap fspace defaultFeatureVec produceFeatures runData = 
    let features:: M.Map q (M.Map d [(Feat, Double)]) 
        features = M.fromListWith (M.unionWith (<>))
                 $ [ ( queryId, 
                        M.fromListWith (<>)
                        [( documentName 
                         , internFeatures fspace $ produceFeatures fname entry
                        )]
                      )
                    | (fname, rankingEntries) <- runData
                    , entry@SimplirRun.RankingEntry {..} <- rankingEntries
                    ]

        featureVectors :: M.Map q (M.Map d (F.FeatureVec Feat ph Double))
        featureVectors =  fmap featureVectorize features           

    in featureVectors
  where featureVectorize :: M.Map d [(Feat, Double)] -> M.Map d (FeatureVec Feat ph Double)
        featureVectorize docFeatureList =
            fmap (F.modify defaultFeatureVec) docFeatureList

