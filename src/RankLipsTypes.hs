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

module RankLipsTypes where

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



type Q = SimplirRun.QueryId
type DocId = SimplirRun.DocumentName
type Rel = IsRelevant
type TrainData f s = M.Map Q [(DocId, FeatureVec f s Double, Rel)]

type FoldRestartResults f s = Folds (M.Map Q [(DocId, FeatureVec f s Double, Rel)],
                                    [(Model f s, Double)])
type BestFoldResults f s = Folds (M.Map Q [(DocId, FeatureVec f s Double, Rel)], (Model f s, Double))




data FeatureVariant = FeatScore | FeatRecipRank
    deriving (Eq, Show, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)  

parseFeatureVariant :: String -> FeatureVariant
parseFeatureVariant str =
  let matches = mapMaybe (\fv ->  if str == show fv then Just fv else Nothing ) [minBound @FeatureVariant .. maxBound]
  in case matches of 
    fv:_ -> fv
    _ -> error $ "count not parse feature variant from "<> str <> ". Valid options: "<> show [minBound @FeatureVariant .. maxBound]


data FeatName = FeatNameInputRun { fRunFile :: FilePath
                                 , featureVariant:: FeatureVariant
                                 }
    deriving (Show, Ord, Eq)

encodeFeatureName :: FeatName -> String
encodeFeatureName FeatNameInputRun{..} = fRunFile <> "-" <> show featureVariant

instance ToJSON FeatName where
    toJSON featNameInputRun = Aeson.toJSON $ encodeFeatureName featNameInputRun

instance ToJSONKey FeatName where
    toJSONKey = contramap encodeFeatureName Aeson.toJSONKey

instance FromJSONKey FeatName where
    fromJSONKey = Aeson.FromJSONKeyTextParser parseFeatName

parseFeatName :: MonadFail m => T.Text -> m FeatName
parseFeatName str =
        case mapMaybe matchEnd $ featureVariant' of
          x : _ -> return x
          _     -> fail $ "Feature name is expected to be of pattern 'filename-FeatureVariant', but got feature name "<> (T.unpack str) <>". Names of accepted FeatureVariants are "<> show [minBound @FeatureVariant .. maxBound]
      where 
          matchEnd :: (FeatureVariant, T.Text) -> Maybe FeatName
          matchEnd (fvariant, fvariant') =
                if fvariant' `T.isSuffixOf` str
                    then 
                        fmap (\s -> FeatNameInputRun {fRunFile = T.unpack s, featureVariant = fvariant} ) $  T.stripSuffix fvariant' str
                    else Nothing

          featureVariant' :: [(FeatureVariant, T.Text)]    
          !featureVariant' = [(fv, T.pack $ "-" <> show fv) | fv <- [minBound @FeatureVariant .. maxBound]]

instance FromJSON FeatName where
    parseJSON (Aeson.String str) = 
      parseFeatName str

    parseJSON x = fail $ "Can only parse FeatName from string values, received "<> show x



data RankLipsModel f s = RankLipsModel { trainedModel :: Model f s
                                       , minibatchParamsOpt :: Maybe MiniBatchParams
                                       , convergenceDiagParameters :: Maybe ConvergenceDiagParams
                                       , useZscore :: Maybe Bool
                                       , cvFold :: Maybe Integer
                                       , heldoutQueries :: Maybe [SimplirRun.QueryId]
                                       , experimentName :: Maybe String
                                       , rankLipsVersion :: Maybe String
                                       , defaultFeatureParams :: Maybe DefaultFeatureParams
                                       }
  

defaultRankLipsModel :: Model f s  -> RankLipsModel f s
defaultRankLipsModel model = RankLipsModel model Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


data DefaultFeatureParams = DefaultFeatureSingleValue { defaultSingleValue :: !Double }
                          | DefaultFeatureVariantValue { defaultFeatureVariant :: [(FeatureVariant, Double)] }
                          | DefaultFeatureValue { defaultFeatureValue :: [(FeatName, Double)] }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data RankLipsMetaField = RankLipsMiniBatch MiniBatchParams 
                       | RankLipsConvergenceDiagParams ConvergenceDiagParams
                       | RankLipsUseZScore Bool
                       | RankLipsCVFold Integer
                       | RankLipsHeldoutQueries [SimplirRun.QueryId]
                       | RankLipsIsFullTrain
                       | RankLipsExperimentName String
                       | RankLipsVersion String
                       | RankLipsDefaultFeatures DefaultFeatureParams
  deriving (Show, Generic, ToJSON, FromJSON)

data RankLipsModelSerialized f = RankLipsModelSerialized { rankLipsTrainedModel :: SomeModel f
                                                          , rankLipsMetaData :: [RankLipsMetaField]
                                                          }
  deriving (Generic, ToJSON, FromJSON)

data SomeRankLipsModel f where 
    SomeRankLipsModel :: RankLipsModel f s -> SomeRankLipsModel f


data FeatureParams = FeatureParams { featureRunsDirectory :: FilePath
                                   , features :: [FilePath]
                                   , featureVariants :: [FeatureVariant]
                                   }
    deriving (Eq, Show)

-- -- | Old-style 'Read'-based JSON representation for feature names.
-- newtype OldFeatureName = OldFeatureName { unOldFeatureName :: Feat }
--   deriving (Eq, Ord, Show)

-- instance FromJSON OldFeatureName where
--     parseJSON (Aeson.String str) =
--         let str' = T.drop 1 $ T.dropEnd 1 str
--         in OldFeatureName . Feat <$> parseFeatName str'

--     parseJSON x = fail $ "Can only parse FeatName from string values, received "<> show x

-- instance FromJSONKey OldFeatureName where
--     fromJSONKey = Aeson.FromJSONKeyTextParser fun
--     -- fromJSONKey = Aeson.FromJSONKeyText fun
--         where fun :: T.Text -> Aeson.Parser OldFeatureName 
--               fun str =
--                 let str' = T.drop 1 $ T.dropEnd 1 str
--                 in OldFeatureName . Feat <$> parseFeatName str'

-- newtype CompatModel = CompatModel (SomeModel Feat)

-- instance FromJSON CompatModel where
--     parseJSON v = newStyle <|> oldStyle
--       where
--         newStyle = fmap CompatModel (Aeson.parseJSON v)
--         oldStyle = fmap (CompatModel . fixFeatureNames) (Aeson.parseJSON v)

--         fixFeatureNames :: SomeModel OldFeatureName -> SomeModel Feat
--         fixFeatureNames (SomeModel model) =
--             case F.mapFeatures fspace (Just . unOldFeatureName) of
--                 F.FeatureMapping fspace' proj ->
--                     let weights' = proj (getWeightVec $ modelWeights' model)
--                     in SomeModel (Model (WeightVec weights'))
--             where
--                 fspace = F.featureSpace $ getWeightVec $ modelWeights' model


-- loadRankLipsV10Model :: FilePath -> IO (RankLipsModelSerialized Feat)
-- loadRankLipsV10Model modelFile = do
--     modelOrErr <- Aeson.eitherDecode @(CompatModel)  <$> BSL.readFile modelFile 
--     case modelOrErr of
--             Right (CompatModel (model :: SomeModel Feat)) -> return $ RankLipsModelSerialized { rankLipsTrainedModel = model, rankLipsMetaData=[] }
--             Left msg -> fail $ "Issue deserializing v1.0 model from file "<> modelFile<> " because: "<> msg <> "."
    


loadRankLipsModel ::  forall f. (FromJSONKey f, Ord f, Show f) => FilePath -> IO (RankLipsModelSerialized f)
loadRankLipsModel modelFile = do
    modelOrErr <- Aeson.eitherDecode @(RankLipsModelSerialized f)  <$> BSL.readFile modelFile 
    case modelOrErr of
            Right model -> return model
            Left msg -> fail $ "Issue deserializing v1.1 model from file "<> modelFile<> " because: "<> msg <> "."




head' :: HasCallStack => [a] -> a
head' (x:_) = x
head' [] = error $ "head': empty list"




newtype Feat = Feat { featureName :: FeatName}
    deriving stock (Eq, Ord)
    deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey )
instance Show Feat where
    show = show . featureName
-- instance Read Feat where
--     readPrec = fmap Feat readPrec


data FeatureSet = FeatureSet { featureNames :: S.Set Feat
                             , produceFeatures :: FilePath -> SimplirRun.RankingEntry -> [(Feat, Double)]
                             }



data ConvergenceDiagParams = ConvergenceDiagParams { convergenceThreshold :: Double
                                                  , convergenceMaxIter :: Int
                                                  , convergenceDropInitIter :: Int
                                                  , convergenceEvalCutoff :: EvalCutoff
                                                  , convergenceRestarts :: Int
                                                  , convergenceFolds :: Int
                                                  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
