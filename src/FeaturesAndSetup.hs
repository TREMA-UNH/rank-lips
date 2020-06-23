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

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T
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


convertFeatureNames :: [FeatureVariant] -> [FilePath] -> S.Set Feat
convertFeatureNames featureVariants features = 
    S.fromList $ [ augmentFname ft run 
                | run <-  features
                , ft <- featureVariants
                ]


augmentFname :: FeatureVariant -> FilePath -> Feat
augmentFname featureVariant fname = Feat $ FeatNameInputRun fname featureVariant



featureSet :: FeatureParams -> FeatureSet
featureSet FeatureParams{..} =
    let
        featureNames :: S.Set Feat
        featureNames = convertFeatureNames featureVariants features 

        produceFeatures :: FilePath -> SimplirRun.RankingEntry -> [(Feat, Double)]
        produceFeatures fname SimplirRun.RankingEntry{..} =
            [ produceFeature ft
            | ft <- featureVariants
            ]
          where produceFeature :: FeatureVariant -> (Feat, Double)
                produceFeature FeatScore = 
                    ((Feat $ FeatNameInputRun fname FeatScore), documentScore)
                produceFeature FeatRecipRank = 
                    ((Feat $ FeatNameInputRun  fname FeatRecipRank), (1.0/(realToFrac documentRank)))  

    in FeatureSet {featureNames=featureNames, produceFeatures = produceFeatures}





createModelEnvelope :: (Model f ph -> Model f ph)
                    -> Maybe String
                    -> Maybe MiniBatchParams
                    -> Maybe ConvergenceDiagParams
                    -> Maybe Bool
                    -> Maybe Bool
                    -> Maybe String
                    -> Maybe DefaultFeatureParams
                    -> (Maybe Integer -> Maybe [SimplirRun.QueryId] -> ModelEnvelope f ph)
createModelEnvelope modelConv experimentName minibatchParamsOpt convergenceDiagParameters useZscore saveHeldoutQueriesInModel version defaultFeatureParams =
    (\cvFold heldOutQueries someModel' -> 
        let trainedModel = modelConv someModel'
            rankLipsModel = RankLipsModel { rankLipsVersion = version
                                          , heldoutQueries = if (fromMaybe False saveHeldoutQueriesInModel) 
                                                                 then heldOutQueries
                                                                 else Nothing
                                          , .. }
        in serializeRankLipsModel rankLipsModel
    )




data QrelInfo = QrelInfo { qrelData :: [QRel.Entry SimplirRun.QueryId SimplirRun.DocumentName IsRelevant]
                         , qrelMap :: M.Map SimplirRun.QueryId (M.Map SimplirRun.DocumentName IsRelevant)
                         , lookupQrel :: IsRelevant -> SimplirRun.QueryId -> SimplirRun.DocumentName -> IsRelevant
                         , totalRels :: M.Map SimplirRun.QueryId Int
                         , metric :: ScoringMetric IsRelevant SimplirRun.QueryId
                         , metricName :: T.Text
                         }
noQrelInfo :: QrelInfo                         
noQrelInfo = QrelInfo { qrelData = []
                      , qrelMap = mempty
                      , lookupQrel = (\rel _q _d -> rel)
                      , totalRels = mempty
                      , metric = const 0.0
                      , metricName = "-"
                      }


loadQrelInfo :: FilePath -> IO QrelInfo
loadQrelInfo qrelFile = do
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

        totalRels :: M.Map SimplirRun.QueryId Int
        !totalRels = fmap countRel qrelMap
                    where countRel :: M.Map x IsRelevant -> Int
                          countRel m = length [ r
                                                | (_, r) <- M.toList m
                                                , QRel.isPositive r
                                                ]

        metric :: ScoringMetric IsRelevant SimplirRun.QueryId
        metric = meanAvgPrec (\q -> fromMaybe 0 $ q `M.lookup` totalRels)  Relevant



    return $ QrelInfo{qrelData = qrelData, qrelMap = qrelMap, lookupQrel = lookupQrel, totalRels = totalRels, metric = metric, metricName = "map"}


createDefaultFeatureVec :: forall ph . F.FeatureSpace Feat ph ->  Maybe (DefaultFeatureParams) -> FeatureVec Feat ph Double
createDefaultFeatureVec fspace defaultFeatureParamsOpt =
        F.fromList fspace 
        $ case  defaultFeatureParamsOpt of
            Just (DefaultFeatureSingleValue val) ->   [ (fname, val)  | fname <- F.featureNames fspace]
            Just (DefaultFeatureVariantValue fvVals) -> [ (f, val )
                                                        | f@Feat{featureName = FeatNameInputRun { featureVariant=fv }} <- F.featureNames fspace
                                                        , (fv', val) <- fvVals
                                                        , fv' == fv
                                                        ]
            Just (DefaultFeatureValue fVals) -> [ (f, val )
                                                        | f@Feat{featureName = fname} <- F.featureNames fspace
                                                        , (fname', val) <- fVals
                                                        , fname' == fname
                                                        ]
            Nothing -> [ (fname, 0.0)  | fname <- F.featureNames fspace]

            x -> error $ "Default feature mode " <> show x <> " is not implemented. Supported: DefaultFeatureSingleValue, DefaultFeatureVariantValue, or DefaultFeatureValue."


doPredict :: forall ph 
            . FeatureParams
            -> FilePath 
            -> Maybe DefaultFeatureParams
            -> Model Feat ph
            -> Maybe FilePath 
            -> IO () 
doPredict featureParams@FeatureParams{..} outputFilePrefix defaultFeatureParamsOpt model qrelFileOpt  = do

    let fspace = modelFeatures model
        defaultFeatureVec = createDefaultFeatureVec fspace defaultFeatureParamsOpt
        FeatureSet {featureNames=_featureNames, produceFeatures=produceFeatures}
           = featureSet featureParams

    runFiles <- loadRunFiles  featureRunsDirectory features
    putStrLn $ " loadRunFiles " <> (unwords $ fmap fst runFiles)

    
    QrelInfo{..} <- fromMaybe noQrelInfo 
             <$> mapM loadQrelInfo qrelFileOpt


    let featureDataMap = runFilesToFeatureVectorsMap fspace defaultFeatureVec produceFeatures runFiles
        featureDataList = fmap M.toList featureDataMap

        allDataList :: M.Map SimplirRun.QueryId [( SimplirRun.DocumentName, FeatureVec Feat ph Double, Rel)]
        allDataList = augmentWithQrelsList_ (lookupQrel NotRelevant) featureDataList

        ranking = withStrategy (parTraversable rseq) 
                $ rerankRankings' model allDataList    

    case qrelFileOpt of
        Just _ -> storeRankingData outputFilePrefix ranking metric "predict"
        Nothing -> storeRankingDataNoMetric outputFilePrefix ranking "predict"


doTrain :: FeatureParams
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
doTrain featureParams@FeatureParams{..} outputFilePrefix experimentName qrelFile miniBatchParams includeCv useZScore saveHeldoutQueriesInModel convergenceParams defaultFeatureParamsOpt rankLipsVersion = do
    let FeatureSet {featureNames=featureNames,  produceFeatures=produceFeatures}
         = featureSet featureParams


    F.SomeFeatureSpace (fspace:: F.FeatureSpace Feat ph) <- pure $ F.mkFeatureSpace featureNames

    runFiles <- loadRunFiles  featureRunsDirectory features
    putStrLn $ " loadRunFiles " <> (unwords $ fmap fst runFiles)

    QrelInfo{..} <- loadQrelInfo qrelFile

    let defaultFeatureVec =  createDefaultFeatureVec fspace defaultFeatureParamsOpt

        featureDataMap = runFilesToFeatureVectorsMap fspace defaultFeatureVec produceFeatures runFiles
        featureDataList :: M.Map SimplirRun.QueryId [( SimplirRun.DocumentName, (F.FeatureVec Feat ph Double))] 
        featureDataList = fmap M.toList featureDataMap

        (featureDataList', createModelEnvelope') =
            if useZScore
                then
                    let zNorm :: Normalisation Feat ph Double
                        zNorm = zNormalizer $ [ feat
                                            | (_, list )<- M.toList featureDataList
                                            , (_, feat ) <- list
                                            ]
                        featureDataListZscore :: M.Map SimplirRun.QueryId [( SimplirRun.DocumentName, (F.FeatureVec Feat ph Double))] 
                        featureDataListZscore = fmap normDocs featureDataList
                          where normDocs list =
                                    [ (doc, (normFeatures zNorm) feat)
                                    | (doc, feat) <- list    
                                    ]

                        modelConv (Model (WeightVec weights)) = Model (WeightVec $ denormWeights zNorm weights)

                    in (featureDataListZscore, createModelEnvelope modelConv)

                else (featureDataList, createModelEnvelope id)

        allDataListRaw :: M.Map SimplirRun.QueryId [( SimplirRun.DocumentName, FeatureVec Feat ph Double, Rel)]
        allDataListRaw = augmentWithQrelsList_ (lookupQrel NotRelevant) featureDataList'

        allDataList = allDataListRaw


        modelEnvelope = createModelEnvelope' (Just experimentName) (Just miniBatchParams) (Just convergenceParams) (Just useZScore) (Just saveHeldoutQueriesInModel) (Just rankLipsVersion) defaultFeatureParamsOpt

    train includeCv fspace allDataList qrelData miniBatchParams convergenceParams  outputFilePrefix modelEnvelope


train :: Bool
      -> F.FeatureSpace Feat ph
      -> TrainData Feat ph
      -> [QRel.Entry SimplirRun.QueryId doc IsRelevant]
      -> MiniBatchParams
      -> ConvergenceDiagParams
      -> FilePath
      -> (Maybe Integer -> Maybe [SimplirRun.QueryId] -> Model Feat ph -> RankLipsModelSerialized Feat)
      -> IO()
train includeCv fspace allData qrel miniBatchParams convergenceDiagParams outputFilePrefix modelEnvelope =  do
    let metric :: ScoringMetric IsRelevant SimplirRun.QueryId
        !metric = meanAvgPrec (totalRelevantFromQRels qrel) Relevant
        totalElems = getSum . foldMap ( Sum . length ) $ allData
        totalPos = getSum . foldMap ( Sum . length . filter (\(_,_,rel) -> rel == Relevant)) $ allData

    putStrLn $ "Feature dimension: "++show (F.dimension $ F.featureSpace $ (\(_,a,_) -> a) $ head' $ snd $ M.elemAt 0 allData)
    putStrLn $ "Training model with (trainData) "++ show (M.size allData) ++
            " queries and "++ show totalElems ++" items total of which "++
            show totalPos ++" are positive."
    let displayTrainData :: Show f => TrainData f ph -> [String]
        displayTrainData trainData =
            [ show k ++ " " ++ show d ++ " " ++ show r ++ " -> "++ prettyFv
            | (k,list) <- M.toList trainData
            , (d,fvec, r) <- list
            , let prettyFv = unlines $ fmap show $ F.toList fvec
            ]

    putStrLn $ "Training Data = \n" ++ intercalate "\n" (take 10 $ displayTrainData $ force allData)
    gen0 <- newStdGen  -- needed by learning to rank
    trainMe includeCv miniBatchParams convergenceDiagParams
            gen0 allData fspace metric outputFilePrefix "" modelEnvelope




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


internFeatures :: F.FeatureSpace Feat ph -> [(Feat, d)] -> [(Feat, d)]
internFeatures fspace features =
    fmap internF features
  where 
    internF (f,v) =  
      let f' = F.internFeatureName fspace f
      in case f' of
            Just f'' -> (f'', v)
            Nothing -> error $ "Trying to intern feature "<> show f <> ", but is not defined in featurespace."


runFilesToFeatureVectorsMap :: forall ph . F.FeatureSpace Feat ph 
                          -> F.FeatureVec Feat ph Double
                          -> (FilePath -> SimplirRun.RankingEntry -> [(Feat, Double)])
                          -> [(FilePath, [SimplirRun.RankingEntry])] 
                          -> M.Map SimplirRun.QueryId (M.Map SimplirRun.DocumentName (F.FeatureVec Feat ph Double))
runFilesToFeatureVectorsMap fspace defaultFeatureVec produceFeatures runData = 
    let features:: M.Map SimplirRun.QueryId (M.Map SimplirRun.DocumentName [(Feat, Double)]) 
        features = M.fromListWith (M.unionWith (<>))
                 $ [ (queryId, 
                        M.fromListWith (<>)
                        [( documentName 
                         , internFeatures fspace $ produceFeatures fname entry
                        )]
                      )
                    | (fname, rankingEntries) <- runData
                    , entry@SimplirRun.RankingEntry {..} <- rankingEntries
                    ]

        featureVectors :: M.Map SimplirRun.QueryId (M.Map SimplirRun.DocumentName (F.FeatureVec Feat ph Double))
        featureVectors =  fmap featureVectorize features           

    in featureVectors
  where featureVectorize :: M.Map SimplirRun.DocumentName [(Feat, Double)] -> M.Map SimplirRun.DocumentName (FeatureVec Feat ph Double)
        featureVectorize docFeatureList =
            fmap (F.modify defaultFeatureVec) docFeatureList

