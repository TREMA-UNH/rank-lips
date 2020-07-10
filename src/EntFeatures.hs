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
import System.FilePath

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Data.Ord
import Data.Bifunctor (Bifunctor(first))
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

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
import JsonRunQrels
import RankDataType

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
                   [ ( queryId, 
                        M.fromListWith (<>)
                        [( documentName' 
                         , scale normalizationFactor featList
                        )]
                      )
                    | (queryId, featMap) <- plainFeats
                    , (documentName, featList) <- M.toList featMap 
                    , let assocDocumentNames = resolveAssocs queryId documentName   -- normalization by participating objects
                    , let normalizationFactor = 1.0 / (realToFrac $ Data.List.length assocDocumentNames)
                    , documentName' <- assocDocumentNames
                    ]


                -- | convert run files into list of features (keyed on q and d)
                producePlainFeatures :: [(FilePath, [SimplirRun.RankingEntry' q d])]  -> [(q, M.Map d [(Feat,Double)])]
                producePlainFeatures runData = 
                           [ ( queryId, 
                                M.fromListWith (<>)
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


resolveAssociations :: Eq q => [SimplirRun.RankingEntry' q RankData] -> q -> RankData -> [RankData]
resolveAssociations assocs query doc =
    [ documentName
        | SimplirRun.RankingEntry {..}<- assocs
        , queryId == query
        , partialMatch doc documentName
        ]
  where partialMatch :: RankData -> RankData -> Bool
        partialMatch (RankData part) (RankData whole) =
            (part `M.isSubmapOf` whole) 
            --   ||  Data.List.all 
            --     [ case (key M.lookup whole) of 
            --             Just vals -> Data.List.all [ v <- val] 
            --     | (key,val) <- M.toList part
            --     ] 
                


doEntTrain :: forall q . (Ord q, Show q, NFData q,  Aeson.FromJSON q, Render q)
            => (RankData -> RankData) 
            -> FeatureParams
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
            -> Maybe RankDataField
            -> String
            -> IO ()
doEntTrain projD featureParams@FeatureParams{..} assocsFile outputFilePrefix experimentName qrelFile miniBatchParams includeCv useZScore saveHeldoutQueriesInModel convergenceParams defaultFeatureParamsOpt trainFieldOpt rankLipsVersion  = do
    let FeatureSet {featureNames=featureNames,  produceFeatures=produceFeatures}
         = featureSet featureParams


    F.SomeFeatureSpace (fspace:: F.FeatureSpace Feat ph) <- pure $ F.mkFeatureSpace featureNames

    runFiles <- RankLips.loadJsonLRunFiles featureRunsDirectory features
    putStrLn $ " loadRunFiles " <> (unwords $ fmap fst runFiles)

    assocs <- readJsonLRunFile assocsFile


    let projectGroundTruth = case trainFieldOpt of
                                Nothing -> id
                                Just field -> fmap (\entry@(QRel.Entry {..}) -> entry { QRel.documentName = projectRankData field documentName }) 

    QrelInfo{..} <- loadQrelInfo . projectGroundTruth <$> readJsonLQrelFile qrelFile
                    
    putStrLn $ " loaded qrels " <> (unlines $ fmap show  $ Data.List.take 10 $ qrelData)

    let defaultFeatureVec =  createEntDefaultFeatureVec fspace defaultFeatureParamsOpt

        featureDataMap = runFilesToEntFeatureVectorsMap fspace defaultFeatureVec (resolveAssociations assocs) produceFeatures runFiles
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

        featureDataListProjected = projectFeatureSpace projD featureDataList'     ----   d -> d'

        allDataListRaw :: M.Map q [( RankData, FeatureVec Feat ph Double, Rel)]
        allDataListRaw = RankLips.augmentWithQrelsList_ (lookupQrel QRel.NotRelevant) featureDataListProjected

        allDataList = allDataListRaw

        modelEnvelope = createModelEnvelope' (Just experimentName) (Just miniBatchParams) (Just convergenceParams) (Just useZScore) (Just saveHeldoutQueriesInModel) (Just rankLipsVersion) defaultFeatureParamsOpt


    putStrLn $ unlines $ fmap show $ Data.List.take 10 $ M.toList allDataList


    train includeCv fspace allDataList qrelData miniBatchParams convergenceParams  outputFilePrefix modelEnvelope




train :: forall ph q d . (Ord q, Ord d, Show q, Show d, NFData q, NFData d, Render q, Render d)
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

        strat :: Strategy [RankLips.TrainedResult f s q d]
        strat = parBuffer 24 rseq
    
    (cvModels, fullModels) <- RankLips.mapCvFull savedTrainedResult
                            $ withStrategy (parTuple2 (parTraversable rseq) rseq)
                            $ ( fmap RankLips.bestRestart  (foldRestartResults)
                              , (RankLips.bestRestart fullRestartResults)
                              )

    let testRanking = RankLips.computeTestRanking  $ cvModels
    _ <- savedTrainedResult testRanking
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

