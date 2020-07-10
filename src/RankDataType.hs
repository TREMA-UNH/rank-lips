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


module RankDataType where

import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Parallel.Strategies (NFData)
import qualified Data.Scientific
import qualified Data.Vector as Vector


import GHC.Generics (Generic)


import RankLipsTypes

newtype RankDataField = RankDataField { unDataField :: T.Text }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Aeson.FromJSON, Aeson.FromJSONKey, Aeson.ToJSON, Aeson.ToJSONKey, NFData)

data RankDataValue = RankDataText {unRankDataText :: T.Text} | RankDataList {unRankDataList :: [T.Text]}
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass ( NFData)

newtype RankData = RankData { unData :: M.Map RankDataField RankDataValue}
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass ( NFData)


instance FromJSON RankData where
    parseJSON (Aeson.Object obj) =
        return $ RankData $ M.fromList [(RankDataField key, parseJSONValue val) | (key, val) <- HM.toList obj]

    parseJSON x@(Aeson.Array arr) = 
        return $  singletonRankData (RankDataField "id") $ parseJSONValue x

    parseJSON x  = 
        return $ singletonRankData (RankDataField "id") $ parseJSONValue x
 
    


parseJSONValue :: Aeson.Value -> RankDataValue 
parseJSONValue (Aeson.String str) = 
  RankDataText str

parseJSONValue (Aeson.Number n) = 
    RankDataText (T.pack $ renderScientific n)
    where renderScientific n = 
            Data.Scientific.formatScientific Data.Scientific.Fixed (Just 0) n

parseJSONValue (Aeson.Bool b) = 
    RankDataText (T.pack $ show b)

parseJSONValue (Aeson.Array arr) = 
    RankDataList $ fmap (T.pack . show) $ Vector.toList arr

parseJSONValue (Aeson.Object obj) =
    error $ "RankData does not support nested objects, but received "<> show obj

parseJSONValue x = error $ "Can't parse nested RankData values from "<> show x


instance ToJSON RankData where
    toJSON (RankData map) = 
        Aeson.toJSON $ fmap unwrapRankDataValue map
      where unwrapRankDataValue (RankDataText t) = Aeson.toJSON t
            unwrapRankDataValue (RankDataList l) = Aeson.toJSON l



    
instance Render RankData where
    render rd@(RankData m) = 
        case M.size m of
            1 -> case head $ M.elems m of
                    RankDataText t -> t
                    RankDataList l -> "[" <> ( T.intercalate ", " l) <> "]"
            0 -> error $ "can't render empty RankData."
            _ -> T.pack $ BSL.unpack $ Aeson.encode rd



modRankData :: (M.Map RankDataField RankDataValue -> M.Map RankDataField RankDataValue) -> RankData -> RankData
modRankData mod (RankData x) =
    RankData $ mod x

singletonRankData :: RankDataField -> RankDataValue -> RankData
singletonRankData field value = 
    RankData $ M.singleton field value

projectRankData :: RankDataField -> RankData -> RankData 
projectRankData field rd = modRankData (M.filterWithKey (\k _ -> k == field) ) rd


