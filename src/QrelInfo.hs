{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QrelInfo where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Maybe

import SimplIR.LearningToRank

import qualified SimplIR.Format.QRel as QRel



data QrelInfo q d = QrelInfo { qrelData :: [QRel.Entry q d QRel.IsRelevant]
                            , qrelMap :: M.Map q (M.Map d QRel.IsRelevant)
                            , lookupQrel :: QRel.IsRelevant -> q -> d -> QRel.IsRelevant
                            , totalRels :: M.Map q Int
                            , metric :: ScoringMetric QRel.IsRelevant q
                            , metricName :: T.Text
                            }
noQrelInfo :: (Ord q) => QrelInfo q d                        
noQrelInfo = QrelInfo { qrelData = []
                      , qrelMap = mempty
                      , lookupQrel = (\rel _q _d -> rel)
                      , totalRels = mempty
                      , metric = const 0.0
                      , metricName = "-"
                      }


loadQrelInfo :: forall q d  . (Ord q, Ord d) -- Show q, Show d)
            =>  [QRel.Entry q d QRel.IsRelevant]
            -> QrelInfo q d
loadQrelInfo qrelData' = 
    let
        qrelMap :: M.Map q (M.Map d QRel.IsRelevant)
        qrelMap = M.fromListWith (<>)
                [ (queryId, M.singleton (documentName) relevance)
                | QRel.Entry {..}  <- qrelData'
                ]

        lookupQrel :: QRel.IsRelevant -> q -> d -> QRel.IsRelevant
        lookupQrel defaultRel queryId docName =
            case queryId `M.lookup` qrelMap of
                Nothing -> defaultRel
                Just dMap -> case  docName `M.lookup` dMap of
                                Nothing -> defaultRel
                                Just r -> r

        totalRels :: M.Map q Int
        !totalRels = fmap countRel qrelMap
                    where countRel :: M.Map x QRel.IsRelevant -> Int
                          countRel m = length [ r
                                                | (_, r) <- M.toList m
                                                , QRel.isPositive r
                                                ]

        metric :: ScoringMetric QRel.IsRelevant q
        metric = meanAvgPrec (\q -> fromMaybe 0 $ q `M.lookup` totalRels)  QRel.Relevant

    in QrelInfo{qrelData = qrelData', qrelMap = qrelMap, lookupQrel = lookupQrel, totalRels = totalRels, metric = metric, metricName = "map"}



loadJsonQrelInfo :: forall q d . (Ord q, Ord d) -- , Show q, Show d)
             => [QRel.Entry q d QRel.IsRelevant] 
             -> IO (QrelInfo q d)
loadJsonQrelInfo qrelData' = do


    let qrelMap :: M.Map q (M.Map d QRel.IsRelevant)
        qrelMap = M.fromListWith (<>)
                [ (queryId, M.singleton (documentName) relevance)
                | QRel.Entry {..}  <- qrelData'
                ]

        lookupQrel :: QRel.IsRelevant -> q -> d -> QRel.IsRelevant
        lookupQrel defaultRel queryId docName =
            case queryId `M.lookup` qrelMap of
                Nothing -> defaultRel
                Just dMap -> case  docName `M.lookup` dMap of
                                Nothing -> defaultRel
                                Just r -> r

        totalRels :: M.Map q Int
        !totalRels = fmap countRel qrelMap
                    where countRel :: M.Map x QRel.IsRelevant -> Int
                          countRel m = length [ r
                                                | (_, r) <- M.toList m
                                                , QRel.isPositive r
                                                ]

        metric :: ScoringMetric QRel.IsRelevant q
        metric = meanAvgPrec (\q -> fromMaybe 0 $ q `M.lookup` totalRels)  QRel.Relevant



    return $ QrelInfo{qrelData = qrelData', qrelMap = qrelMap, lookupQrel = lookupQrel, totalRels = totalRels, metric = metric, metricName = "map"}
