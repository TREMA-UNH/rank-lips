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

module RankLipsCompat where

import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.List
import Data.Foldable as Foldable
import Data.Maybe
import Data.Coerce

import Control.Applicative
import Data.Function
import Data.Bifunctor
import System.Random
import Control.Parallel.Strategies
import Control.Concurrent.Map
import Control.DeepSeq

import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import SimplIR.FeatureSpace (FeatureSpace, FeatureVec)
import qualified SimplIR.FeatureSpace as F

import qualified SimplIR.Format.QRel as QRel
import qualified SimplIR.Ranking as Ranking
import SimplIR.TrainUtils

import Debug.Trace as Debug

import qualified SimplIR.Format.TrecRunFile as SimplirRun
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Data.Functor.Contravariant (Contravariant(contramap))
import qualified Data.Aeson.Types as Aeson

import RankLipsTypes


-- | Old-style 'Read'-based JSON representation for feature names.
newtype OldFeatureName = OldFeatureName { unOldFeatureName :: Feat }
  deriving (Eq, Ord, Show)

instance FromJSON OldFeatureName where
    parseJSON (Aeson.String str) =
        let str' = T.drop 1 $ T.dropEnd 1 str
        in OldFeatureName . Feat <$> parseFeatName str'

    parseJSON x = fail $ "Can only parse FeatName from string values, received "<> show x

instance FromJSONKey OldFeatureName where
    fromJSONKey = Aeson.FromJSONKeyTextParser fun
    -- fromJSONKey = Aeson.FromJSONKeyText fun
        where fun :: T.Text -> Aeson.Parser OldFeatureName 
              fun str =
                let str' = T.drop 1 $ T.dropEnd 1 str
                in OldFeatureName . Feat <$> parseFeatName str'

newtype CompatModel = CompatModel (SomeModel Feat)

instance FromJSON CompatModel where
    parseJSON v = newStyle <|> oldStyle
      where
        newStyle = fmap CompatModel (Aeson.parseJSON v)
        oldStyle = fmap (CompatModel . fixFeatureNames) (Aeson.parseJSON v)

        fixFeatureNames :: SomeModel OldFeatureName -> SomeModel Feat
        fixFeatureNames (SomeModel model) =
            case F.mapFeatures fspace (Just . unOldFeatureName) of
                F.FeatureMapping fspace' proj ->
                    let weights' = proj (getWeightVec $ modelWeights' model)
                    in SomeModel (Model (WeightVec weights'))
            where
                fspace = F.featureSpace $ getWeightVec $ modelWeights' model


loadRankLipsV10Model :: FilePath -> IO (RankLipsModelSerialized Feat)
loadRankLipsV10Model modelFile = do
    modelOrErr <- Aeson.eitherDecode @(CompatModel)  <$> BSL.readFile modelFile 
    case modelOrErr of
            Right (CompatModel (model :: SomeModel Feat)) -> return $ RankLipsModelSerialized { rankLipsTrainedModel = model, rankLipsMetaData=[] }
            Left msg -> fail $ "Issue deserializing v1.0 model from file "<> modelFile<> " because: "<> msg <> "."
    
