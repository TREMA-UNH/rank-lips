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
{-# LANGUAGE AllowAmbiguousTypes #-}

module EntFeatures where

import Control.DeepSeq hiding (rwhnf)
import Control.Parallel.Strategies
import Data.Semigroup hiding (All, Any, option)
import System.Random

import qualified Data.Map.Strict as M
import Data.List

import qualified Data.Aeson as Aeson
import Data.List.NonEmpty as NE

import qualified SimplIR.Format.TrecRunFile as SimplirRun
import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (FeatureVec)
import SimplIR.FeatureSpace.Normalise

import qualified SimplIR.Format.QRel as QRel

import RankLipsTypes
import qualified FeaturesAndSetup as RankLips
import qualified TrainAndSave as RankLips
import QrelInfo
import RankLipsFeatureUtils
import SimplIR.Format.JsonRunQrels
import RankDataType
import Control.Monad (when, void)
import GHC.Stack (HasCallStack)
import qualified Debug.Trace as Debug
import qualified Data.Text as T
import Data.Maybe
import qualified Data.Set as S

scale :: Double -> [(Feat,Double)] ->  [(Feat,Double)] 
scale x feats = 
    [ (f, x * v) | (f, v) <- feats]

runFilesToEntFeatureVectorsMap :: forall ph q d . (Ord q, Ord d)
                          =>  F.FeatureSpace Feat ph 
                          -> F.FeatureVec Feat ph Double
                          -> (q -> d -> [d])
                          -> (FilePath -> SimplirRun.RankingEntry' q d -> [(Feat, Double)])
                          -> [(FilePath, [SimplirRun.RankingEntry' q d])] 
                          -> M.Map q (M.Map d (F.FeatureVec Feat ph Double))
runFilesToEntFeatureVectorsMap fspace defaultFeatureVec resolveAssocs produceFeatures runData = 
    let features:: M.Map q (M.Map d [(Feat, Double)]) 
        features = M.fromListWith (M.unionWith (<>))
          $ projectFeatures
          $ producePlainFeatures runData

           
          where 
                -- | project out fields in features using associations
                projectFeatures ::  [(q, M.Map d [(Feat,Double)])] -> [(q, M.Map d [(Feat,Double)])]
                projectFeatures plainFeats = 
                    M.toList                  --- <-- build map
                    $ M.mapWithKey proj 
                    $ M.fromListWith (M.union) plainFeats
                  where proj queryId featMap =  
                                M.fromListWith (<>)   --- <-- build map
                                $ withStrategy (parBuffer 20 r0)
                                $  [   
                                        ( documentName' 
                                        , scale normalizationFactor featList
                                        )
                                    | (documentName, featList) <- M.toList featMap 
                                    , let assocDocumentNames = resolveAssocs queryId documentName   --  <-- sloooow?
                                    , let normalizationFactor = 1.0 / (realToFrac $ Data.List.length assocDocumentNames)
                                    , documentName' <- assocDocumentNames
                                    ]
                                -- Remarks: 
                                --    * documentName is the name associated with a feature from a runfile (e.g. neighbor/paragraph combination)
                                --    * documentName' is the name of the ranking we seek to produce (e.g. entity)


                -- | convert run files into list of features (keyed on q and d)
                producePlainFeatures :: [(FilePath, [SimplirRun.RankingEntry' q d])]  -> [(q, M.Map d [(Feat,Double)])]
                producePlainFeatures runData = 
                           [ ( queryId, 
                                M.fromListWith (<>)   -- <-- build map
                                [( documentName 
                                , (internFeatures fspace 
                                  $ produceFeatures fname entry
                                  )
                                 )] :: M.Map d [(Feat, Double)]
                            )
                            | (fname:: FilePath, rankingEntries:: [SimplirRun.RankingEntry' q d]) <- runData
                            , (entry@(SimplirRun.RankingEntry {..}) :: SimplirRun.RankingEntry' q d) <- rankingEntries
                            ]



        featureVectors :: M.Map q (M.Map d (F.FeatureVec Feat ph Double))
        featureVectors =  fmap featureVectorize features           

    in featureVectors
  where featureVectorize :: M.Map d [(Feat, Double)] -> M.Map d (FeatureVec Feat ph Double)
        featureVectorize docFeatureList =
            fmap (F.modify defaultFeatureVec) docFeatureList


createEntDefaultFeatureVec :: forall ph . F.FeatureSpace Feat ph ->  Maybe (DefaultFeatureParams) -> FeatureVec Feat ph Double
createEntDefaultFeatureVec fspace defaultFeatureParamsOpt =
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


resolveAssociations :: (Eq q, Show q, Ord q) => S.Set RankDataField -> [SimplirRun.RankingEntry' q RankData] -> q -> RankData -> [RankData]
resolveAssociations predictFields assocs =
    let assocIdx = M.fromListWith (<>)
                 $  [ (queryId, [documentName])
                        | SimplirRun.RankingEntry {..}<- assocs
                        ]
    in \query doc ->
        let res =   [ predictProj documentName
                    | let rds = fromMaybe (error $ "no associations for query "<> (show query) <> " in assocIdx "<> (show assocIdx))
                            $ query `M.lookup` assocIdx
                    , documentName <-  rds
                    , partialMatch doc documentName
                    ]
        in res
        -- in if (null res)
            --    then Debug.trace ("No assocs for query "<> show query<> ", document "<> show doc) $ res        
            --    else res
            -- --    else Debug.trace ("Found Assoc query "<> show query<> ", document "<> show doc) $ res

  where partialMatch :: RankData -> RankData -> Bool
        partialMatch (RankData part) (RankData whole) =
            -- debugTr ("partialMatch part: "<> displayMap part <> "\n  whole "<> displayMap whole) $
            M.isSubmapOfBy equalsOrContains part whole
        displayMap :: M.Map RankDataField RankDataValue -> String
        displayMap m =
            T.unpack $ T.unlines [ "("<> k <> ": "<> display v <> ")"| (RankDataField k,v) <- M.toList m]
    
    
        predictProj :: RankData -> RankData
        predictProj rd =
            modRankData (\m -> M.filterWithKey (\k v -> k `S.member` predictFields) m) rd


debugTr x y = Debug.trace (x <> show y) y



doEntTrain :: forall q . (Ord q, Show q, NFData q,  Aeson.FromJSON q, Render q)
            => FeatureParams
            -> FilePath
            -> FilePath 
            -> FilePath 
            -> FilePath 
            -> MiniBatchParams
            -> Bool
            -> Bool
            -> Bool
            -> ConvergenceDiagParams
            -> Maybe DefaultFeatureParams
            -> S.Set RankDataField
            -> String
            -> IO ()
doEntTrain featureParams@FeatureParams{..} assocsFile outputFilePrefix experimentName qrelFile miniBatchParams 
           includeCv useZScore saveHeldoutQueriesInModel convergenceParams defaultFeatureParamsOpt predictionFields rankLipsVersion  = do
    let FeatureSet {featureNames=featureNames,  produceFeatures=produceFeatures}
         = featureSet featureParams


    F.SomeFeatureSpace (fspace:: F.FeatureSpace Feat ph) <- pure $ F.mkFeatureSpace featureNames

    runFiles <- RankLips.loadJsonLRunFiles featuresRunFormat featureRunsDirectory features
    putStrLn $ " loadRunFiles " <> (unwords $ fmap fst runFiles)

    assocs <- readJsonLRunFile assocsFile


    -- let projectGroundTruth = case trainFieldOpt of
    --                             Nothing -> id
    --                             Just field -> fmap (\entry@(QRel.Entry {..}) -> entry { QRel.documentName = projectRankData field documentName }) 
    let projectGroundTruth = id
    QrelInfo{..} <- loadQrelInfo . projectGroundTruth <$> readJsonLQrelFile qrelFile
                    
    putStrLn $ " loaded qrels " <> (unlines $ fmap show  $ Data.List.take 10 $ qrelData)

    let defaultFeatureVec =  createEntDefaultFeatureVec fspace defaultFeatureParamsOpt

        featureDataMap = runFilesToEntFeatureVectorsMap fspace defaultFeatureVec (resolveAssociations predictionFields assocs) produceFeatures runFiles
        featureDataList :: M.Map q [( RankData, (F.FeatureVec Feat ph Double))] 
        featureDataList = fmap M.toList featureDataMap

        (featureDataList', createModelEnvelope') =
            if useZScore
                then
                    let zNorm :: Normalisation Feat ph Double
                        zNorm = zNormalizer $ [ feat
                                            | (_, list )<- M.toList featureDataList
                                            , (_, feat ) <- list
                                            ]
                        featureDataListZscore :: M.Map q [( RankData, (F.FeatureVec Feat ph Double))] 
                        featureDataListZscore = fmap normDocs featureDataList
                          where normDocs list =
                                    [ (doc, (normFeatures zNorm) feat)
                                    | (doc, feat) <- list    
                                    ]

                        modelConv (Model (WeightVec weights)) = Model (WeightVec $ denormWeights zNorm weights)

                    in (featureDataListZscore, RankLips.createModelEnvelope modelConv)

                else (featureDataList, RankLips.createModelEnvelope id)


    putStrLn $ "featureDataList' " <> show featureDataList'
    let
        -- featureDataListProjected = projectFeatureSpace projD featureDataList'     ----   d -> d'

        allDataListRaw :: M.Map q [( RankData, FeatureVec Feat ph Double, Rel)]
        allDataListRaw = RankLips.augmentWithQrelsList_ (lookupQrel QRel.NotRelevant) featureDataList'

        allDataList = allDataListRaw

        modelEnvelope = createModelEnvelope' (Just experimentName) (Just miniBatchParams) (Just convergenceParams) (Just useZScore) (Just saveHeldoutQueriesInModel) (Just rankLipsVersion) defaultFeatureParamsOpt

    putStrLn $ "Sample of allDataList \n" <> ( unlines $ fmap show $ Data.List.take 10 $ M.toList allDataList)
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
        totalElems = getSum . foldMap ( Sum . Data.List.length ) $ allData
        totalPos = getSum . foldMap ( Sum . Data.List.length . Data.List.filter (\(_,_,rel) -> rel == Relevant)) $ allData

    when (M.null allData) (fail "No features could be constructed")
    when (null $ snd $ M.elemAt 0 allData) (fail "First query does not have any features")


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

    putStrLn $ "Training Data = \n" ++ intercalate "\n" (Data.List.take 10 $ displayTrainData $ force allData)
    gen0 <- newStdGen  -- needed by learning to rank

    let experimentName = ""

    putStrLn "made folds"
    let (fullRestartResults, foldRestartResults) =
            RankLips.trainMe miniBatchParams convergenceDiagParams gen0 allData fspace metric

    let savedTrainedResult model = do
            RankLips.storeModelAndRanking outputFilePrefix experimentName modelEnvelope model
            return model

        (cvComputation, fullComputation) = withStrategy (parTuple2 (parTraversable rseq) rseq)
                                          $ RankLips.mapCvFull RankLips.bestRestart (foldRestartResults, fullRestartResults)

    if includeCv 
    then do
        (cvModels, fullModels) <- RankLips.mapIOCvFull savedTrainedResult (cvComputation, fullComputation)
        let testRanking = RankLips.computeTestRanking  $ cvModels
        void $ savedTrainedResult testRanking
    else do
        void $ savedTrainedResult fullComputation

    return ()





projectFeatureSpace :: forall q d d' f ph . (Ord f, Ord d')
                    => (d -> d')
                    -> M.Map q [( d, FeatureVec f ph Double)] 
                    -> M.Map q [( d', FeatureVec f ph Double)] 
projectFeatureSpace proj allDataList =
    let allDataListProjected = fmap projectList allDataList
    in allDataListProjected
  where projectList :: [(d, FeatureVec f ph Double)] -> [(d', FeatureVec f ph Double)]    
        projectList features =
            M.toList
            $ fmap (F.aggregateWith (+))
            $ M.fromListWith (<>)
            $ [ (proj doc, NE.fromList [feat]) | (doc, feat) <- features]

