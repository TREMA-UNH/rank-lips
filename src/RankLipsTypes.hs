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
import Data.Maybe

import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import SimplIR.FeatureSpace (FeatureSpace, FeatureVec)
import SimplIR.TrainUtils

import Debug.Trace as Debug

import qualified SimplIR.Format.TrecRunFile as SimplirRun
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Data.Functor.Contravariant (Contravariant(contramap))



type Q = SimplirRun.QueryId
type DocId = SimplirRun.DocumentName
type Rel = IsRelevant
type TrainData f s q d = M.Map q [(d, FeatureVec f s Double, Rel)]

type FoldRestartResults f s q d = Folds (M.Map q [(d, FeatureVec f s Double, Rel)],
                                    [(Model f s, Double)])
type BestFoldResults f s q d = Folds (M.Map q [(d, FeatureVec f s Double, Rel)], (Model f s, Double))


class Render a where 
    render :: a -> T.Text


instance Render T.Text where
    render = id


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
                                       , heldoutQueries :: Maybe [String]
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
                       | RankLipsHeldoutQueries [String]
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
                                   , featuresFromJsonL :: Bool
                                   }
    deriving (Eq, Show)


loadRankLipsModel ::  forall f. (FromJSONKey f, Ord f, Show f) => FilePath -> IO (RankLipsModelSerialized f)
loadRankLipsModel modelFile = do
    modelOrErr <- Aeson.eitherDecode @(RankLipsModelSerialized f)  <$> BSL.readFile modelFile 
    case modelOrErr of
            Right model -> return model
            Left msg -> fail $ "Issue deserializing v1.1 model from file "<> modelFile<> " because: "<> msg <> ". To load a v1.0 model, please include flag --is-v10-model"




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


data FeatureSet q d = FeatureSet { featureNames :: S.Set Feat
                             , produceFeatures :: FilePath -> SimplirRun.RankingEntry' q d -> [(Feat, Double)]
                             }



data ConvergenceDiagParams = ConvergenceDiagParams { convergenceThreshold :: Double
                                                  , convergenceMaxIter :: Int
                                                  , convergenceDropInitIter :: Int
                                                  , convergenceEvalCutoff :: EvalCutoff
                                                  , convergenceRestarts :: Int
                                                  , convergenceFolds :: Int
                                                  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)





serializeRankLipsModel :: forall f ph. RankLipsModel f ph -> RankLipsModelSerialized f
serializeRankLipsModel RankLipsModel{..} =
    let rankLipsTrainedModel = SomeModel trainedModel
        rankLipsMetaData = catMaybes [ fmap RankLipsMiniBatch minibatchParamsOpt
                                         , fmap RankLipsConvergenceDiagParams convergenceDiagParameters
                                         , fmap RankLipsUseZScore useZscore
                                         , fmap RankLipsExperimentName experimentName
                                         , fmap RankLipsCVFold cvFold, if cvFold == Nothing then Just RankLipsIsFullTrain else Nothing
                                         , fmap RankLipsHeldoutQueries heldoutQueries
                                         , fmap RankLipsVersion rankLipsVersion
                                         , fmap RankLipsDefaultFeatures defaultFeatureParams
                                         ]
        in RankLipsModelSerialized{..}



deserializeRankLipsModel ::  RankLipsModelSerialized f -> SomeRankLipsModel f
deserializeRankLipsModel RankLipsModelSerialized{..} =
    case rankLipsTrainedModel of
      SomeModel trainedModel -> 
        let    rankLipsModel  = foldl' readMeta (defaultRankLipsModel trainedModel) rankLipsMetaData
        in SomeRankLipsModel rankLipsModel
  where readMeta :: RankLipsModel f ph -> RankLipsMetaField -> RankLipsModel f ph
        readMeta rlm metafield =
          case metafield of
            RankLipsMiniBatch params -> rlm {minibatchParamsOpt = Just params}
            RankLipsUseZScore flag -> rlm {useZscore = Just flag}
            RankLipsCVFold fold -> rlm {cvFold = Just fold}
            RankLipsIsFullTrain -> rlm {cvFold = Nothing}
            RankLipsExperimentName name -> rlm {experimentName = Just name}
            RankLipsHeldoutQueries queries -> rlm {heldoutQueries = Just queries}
            RankLipsConvergenceDiagParams params -> rlm {convergenceDiagParameters = Just params}
            RankLipsVersion version -> rlm {rankLipsVersion = Just version}
            RankLipsDefaultFeatures params -> rlm {defaultFeatureParams = Just params}
            x -> error $ "Don't know how to read metadata field "<> (show x)


