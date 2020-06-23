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
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T


import Control.Applicative


import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F


import qualified Data.Aeson.Types as Aeson

import RankLipsTypes


-- | Old-style 'Read'-based JSON representation for feature names.
newtype OldFeatureName = OldFeatureName { unOldFeatureName :: Feat }
  deriving (Eq, Ord, Show)

instance Aeson.FromJSON OldFeatureName where
    parseJSON (Aeson.String str) =
        let str' = T.drop 1 $ T.dropEnd 1 str
        in OldFeatureName . Feat <$> parseFeatName str'

    parseJSON x = fail $ "Can only parse FeatName from string values, received "<> show x

instance Aeson.FromJSONKey OldFeatureName where
    fromJSONKey = Aeson.FromJSONKeyTextParser fun
    -- fromJSONKey = Aeson.FromJSONKeyText fun
        where fun :: T.Text -> Aeson.Parser OldFeatureName 
              fun str =
                let str' = T.drop 1 $ T.dropEnd 1 str
                in OldFeatureName . Feat <$> parseFeatName str'

newtype CompatModel = CompatModel (SomeModel Feat)

instance Aeson.FromJSON CompatModel where
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
    
