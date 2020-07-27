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


module RankLipsFeatureUtils where


import qualified Data.Set as S

import qualified SimplIR.Format.TrecRunFile as SimplirRun
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (FeatureVec)

import RankLipsTypes
import Data.Maybe (mapMaybe)

convertFeatureNames :: [FeatureVariant] -> [FilePath] -> S.Set Feat
convertFeatureNames featureVariants features = 
    S.fromList $ [ augmentFname ft run 
                | run <-  features
                , ft <- featureVariants
                ]


augmentFname :: FeatureVariant -> FilePath -> Feat
augmentFname featureVariant fname = Feat $ FeatNameInputRun fname featureVariant



featureSet :: FeatureParams -> FeatureSet q d
featureSet FeatureParams{..} =
    let
        featureNames :: S.Set Feat
        featureNames = convertFeatureNames featureVariants features 

        produceFeatures :: FilePath -> SimplirRun.RankingEntry' q d -> [(Feat, Double)]
        produceFeatures fname SimplirRun.RankingEntry{..} = {-# SCC "produceFeatures" #-}
            [ produceFeature ft
            | ft <- featureVariants
            ]
          where produceFeature :: FeatureVariant -> (Feat, Double)
                produceFeature FeatScore = 
                    ((Feat $ FeatNameInputRun fname FeatScore), documentScore)
                produceFeature FeatRecipRank = 
                    ((Feat $ FeatNameInputRun  fname FeatRecipRank), (1.0/(realToFrac documentRank)))  

    in FeatureSet {featureNames=featureNames, produceFeatures = produceFeatures}



internFeatures :: F.FeatureSpace Feat ph -> [(Feat, d)] -> [(Feat, d)]
internFeatures fspace features =
    mapMaybe internF features
  where 
    internF (f,v) =  
      let f' = F.internFeatureName fspace f
      in case f' of
            Just f'' -> Just (f'', v)
            Nothing -> Nothing --  $ "Trying to intern feature "<> show f <> ", but is not defined in featurespace."



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

