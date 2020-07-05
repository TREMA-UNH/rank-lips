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

newtype RankData = RankData { unData :: M.Map RankDataField T.Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype ( Aeson.ToJSON, NFData)


instance FromJSON RankData where
    parseJSON (Aeson.String str) = 
        return $ singletonRankData (RankDataField "id") str
 
    parseJSON (Aeson.Number n) = 
        return $  singletonRankData (RankDataField "id") (T.pack $ renderScientific n)
      where renderScientific n = 
                Data.Scientific.formatScientific Data.Scientific.Fixed (Just 0) n
 
    parseJSON (Aeson.Bool b) = 
        return $  singletonRankData (RankDataField "id") (T.pack $ show b)
 
    parseJSON (Aeson.Array arr) = 
        return $  singletonRankData (RankDataField "id") (T.intercalate "," $ fmap (T.pack . show) $ Vector.toList arr)
 
    parseJSON (Aeson.Object obj) =
        return $ RankData $ M.fromList [(RankDataField key, parseJSONValue val) | (key, val) <- HM.toList obj]

     where
      parseJSONValue (Aeson.String str) = 
        str
 
      parseJSONValue (Aeson.Number n) = 
        (T.pack $ renderScientific n)
       where renderScientific n = 
                Data.Scientific.formatScientific Data.Scientific.Fixed (Just 0) n

      parseJSONValue (Aeson.Bool b) = 
        (T.pack $ show b)
 
      parseJSONValue (Aeson.Array arr) = 
        (T.intercalate "," $ fmap (T.pack . show) $ Vector.toList arr)
 
      parseJSONValue (Aeson.Object obj) =
        error $ "RankData does not support nested objects, but received "<> show obj

      parseJSONValue x = error $ "Can't parse nested RankData values from "<> show x

    parseJSON x = fail $ "Can't parse RankData from "<> show x

instance Render RankData where
    render rd@(RankData m) = 
        case M.size m of
            1 -> head $ M.elems m
            0 -> error $ "can't render empty RankData."
            _ -> T.pack $ BSL.unpack $ Aeson.encode rd



modRankData :: (M.Map RankDataField T.Text -> M.Map RankDataField T.Text) -> RankData -> RankData
modRankData mod (RankData x) =
    RankData $ mod x

singletonRankData :: RankDataField -> T.Text -> RankData
singletonRankData field value = 
    RankData $ M.singleton field value

projectRankData :: RankDataField -> RankData -> RankData 
projectRankData field rd = modRankData (M.filterWithKey (\k _ -> k == field) ) rd


