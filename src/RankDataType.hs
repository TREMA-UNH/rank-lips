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
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Parallel.Strategies (NFData)
import qualified Data.Scientific
import qualified Data.Vector as Vector
import qualified Data.List

import GHC.Generics (Generic)
import Control.Monad ((<=<))


import RankLipsTypes

newtype RankDataField = RankDataField { unDataField :: T.Text }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Aeson.FromJSON, Aeson.FromJSONKey, Aeson.ToJSON, Aeson.ToJSONKey, NFData)

data RankDataValue = RankDataText {unRankDataText :: T.Text} | RankDataList {unRankDataList :: [T.Text]}
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass ( NFData)

display :: RankDataValue -> T.Text
display (RankDataText txt) = txt
display (RankDataList lst) = T.intercalate " " lst


newtype RankData = RankData { unData :: M.Map RankDataField RankDataValue}
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass ( NFData)

instance Aeson.FromJSON RankData where
    parseJSON (Aeson.Object obj) = do
        let f (key, val) = do
              val' <- parseJSONValue val
              return (RankDataField key, val')
        RankData . M.fromList <$> mapM f (HM.toList obj)

    parseJSON x@(Aeson.Array arr) = 
        singletonRankData (RankDataField "id") <$> parseJSONValue x

    parseJSON x  = 
        singletonRankData (RankDataField "id") <$> parseJSONValue x
 

parseJSONValue :: Aeson.Value -> Parser RankDataValue 
parseJSONValue (Aeson.String str) = 
    return $ RankDataText str

parseJSONValue (Aeson.Number n) = 
    return $ RankDataText (T.pack $ renderScientific n)
    where renderScientific n = 
            Data.Scientific.formatScientific Data.Scientific.Fixed (Just 0) n

parseJSONValue (Aeson.Bool b) = 
    return $ RankDataText (T.pack $ show b)

parseJSONValue (Aeson.Array arr) = 
    RankDataList <$> mapM (\x -> f =<< parseJSONValue x) (Vector.toList arr)
  where
    f :: RankDataValue -> Parser T.Text
    f (RankDataText txt) = return txt
    f (RankDataList lst) = fail $ "Lists of lists are not supported, but received "<> show arr


parseJSONValue (Aeson.Object obj) =
    fail $ "RankData does not support nested objects, but received "<> show obj

parseJSONValue x = fail $ "Can't parse nested RankData values from "<> show x


instance Aeson.ToJSON RankData where
    toJSON (RankData m) = 
        Aeson.toJSON $ fmap unwrapRankDataValue m
      where unwrapRankDataValue (RankDataText t) = Aeson.toJSON t
            unwrapRankDataValue (RankDataList l) = Aeson.toJSON l

ofListType :: RankDataValue -> Bool
ofListType (RankDataList _) = True
ofListType (RankDataText _) = False

equalsOrContains :: RankDataValue -> RankDataValue -> Bool
equalsOrContains (RankDataList part) (RankDataList whole) = 
    all ( `Data.List.elem` whole) part

equalsOrContains (RankDataText part) (RankDataList whole) = 
    part `Data.List.elem` whole

equalsOrContains (RankDataList part) (RankDataText whole) = 
    case part of
            [p] -> whole == p
            _ -> False

equalsOrContains (RankDataText part) (RankDataText whole) = 
    part == whole

rankDataLookup :: RankDataField -> RankData -> Maybe RankData
rankDataLookup field (RankData m) =
    fmap (\v -> RankData $ M.singleton field v ) $ field `M.lookup` m 

    
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


