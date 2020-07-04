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
import FeaturesAndSetup
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
          where projectFeatures ::  [(q, M.Map d [(Feat,Double)])] -> [(q, M.Map d [(Feat,Double)])]
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


resolveAssocs :: Eq q => [SimplirRun.RankingEntry' q RankData] -> q -> RankData -> [RankData]
resolveAssocs assocs query doc =
    [ documentName
        | SimplirRun.RankingEntry {..}<- assocs
        , queryId == query
        , partialMatch doc documentName
        ]
  where partialMatch :: RankData -> RankData -> Bool
        partialMatch (RankData part) (RankData whole) =
            part `M.isSubmapOf` whole


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

    runFiles <- loadJsonLRunFiles featureRunsDirectory features
    putStrLn $ " loadRunFiles " <> (unwords $ fmap fst runFiles)

    assocs <- readJsonLRunFile assocsFile


    let projectGroundTruth = case trainFieldOpt of
                                Nothing -> id
                                Just field -> fmap (\entry@(QRel.Entry {..}) -> entry { QRel.documentName = projectRankData field documentName }) 

    QrelInfo{..} <- loadQrelInfo . projectGroundTruth <$> readJsonLQrelFile qrelFile
                    
    putStrLn $ " loaded qrels " <> (unlines $ fmap show  $ Data.List.take 10 $ qrelData)

    let defaultFeatureVec =  createEntDefaultFeatureVec fspace defaultFeatureParamsOpt

        featureDataMap = runFilesToEntFeatureVectorsMap fspace defaultFeatureVec (resolveAssocs assocs) produceFeatures runFiles
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

                    in (featureDataListZscore, createModelEnvelope modelConv)

                else (featureDataList, createModelEnvelope id)

        featureDataListProjected = projectFeatureSpace projD featureDataList'     ----   d -> d'

        allDataListRaw :: M.Map q [( RankData, FeatureVec Feat ph Double, Rel)]
        allDataListRaw = augmentWithQrelsList_ (lookupQrel QRel.NotRelevant) featureDataListProjected

        allDataList = allDataListRaw

        modelEnvelope = createModelEnvelope' (Just experimentName) (Just miniBatchParams) (Just convergenceParams) (Just useZScore) (Just saveHeldoutQueriesInModel) (Just rankLipsVersion) defaultFeatureParamsOpt


    putStrLn $ unlines $ fmap show $ Data.List.take 10 $ M.toList allDataList


    train includeCv fspace allDataList qrelData miniBatchParams convergenceParams  outputFilePrefix modelEnvelope



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

