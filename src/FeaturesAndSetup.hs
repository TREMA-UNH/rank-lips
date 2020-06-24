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


module FeaturesAndSetup where

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

import qualified SimplIR.Format.TrecRunFile as SimplirRun
import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (FeatureVec)
import SimplIR.FeatureSpace.Normalise

import qualified SimplIR.Format.QRel as QRel

import TrainAndSave
import RankLipsTypes
import Data.Bifunctor (Bifunctor(second))
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL


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
        produceFeatures fname SimplirRun.RankingEntry{..} =
            [ produceFeature ft
            | ft <- featureVariants
            ]
          where produceFeature :: FeatureVariant -> (Feat, Double)
                produceFeature FeatScore = 
                    ((Feat $ FeatNameInputRun fname FeatScore), documentScore)
                produceFeature FeatRecipRank = 
                    ((Feat $ FeatNameInputRun  fname FeatRecipRank), (1.0/(realToFrac documentRank)))  

    in FeatureSet {featureNames=featureNames, produceFeatures = produceFeatures}





createModelEnvelope :: (Show q)
                    => (Model f ph -> Model f ph)
                    -> Maybe String
                    -> Maybe MiniBatchParams
                    -> Maybe ConvergenceDiagParams
                    -> Maybe Bool
                    -> Maybe Bool
                    -> Maybe String
                    -> Maybe DefaultFeatureParams
                    -> (Maybe Integer -> Maybe [q] -> ModelEnvelope f ph)
createModelEnvelope modelConv experimentName minibatchParamsOpt convergenceDiagParameters useZscore saveHeldoutQueriesInModel version defaultFeatureParams =
    (\cvFold heldOutQueries someModel' -> 
        let trainedModel = modelConv someModel'
            rankLipsModel = RankLipsModel { rankLipsVersion = version
                                          , heldoutQueries = if (fromMaybe False saveHeldoutQueriesInModel) 
                                                                 then fmap (fmap show) heldOutQueries
                                                                 else Nothing
                                          , .. }
        in serializeRankLipsModel rankLipsModel
    )




data QrelInfo q d = QrelInfo { qrelData :: [QRel.Entry q d IsRelevant]
                         , qrelMap :: M.Map q (M.Map d IsRelevant)
                         , lookupQrel :: IsRelevant -> q -> d -> IsRelevant
                         , totalRels :: M.Map q Int
                         , metric :: ScoringMetric IsRelevant q
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


loadQrelInfo :: forall q d . (Ord q, Ord d, Show q, Show d)
             =>  (SimplirRun.QueryId -> q) 
             -> (SimplirRun.DocumentName -> d) 
             -> FilePath 
             -> IO (QrelInfo q d)
loadQrelInfo convQ convD qrelFile = do
    qrelData <- QRel.readQRel qrelFile
                :: IO [QRel.Entry SimplirRun.QueryId SimplirRun.DocumentName IsRelevant]


    let qrelData' :: [QRel.Entry q d IsRelevant]
        qrelData' = [ QRel.Entry {queryId = (convQ queryId), documentName = (convD documentName), relevance = relevance} | QRel.Entry {..}  <- qrelData]
        qrelMap :: M.Map q (M.Map d IsRelevant)
        qrelMap = M.fromListWith (<>)
                [ (queryId, M.singleton (documentName) relevance)
                | QRel.Entry {..}  <- qrelData'
                ]

        lookupQrel :: IsRelevant -> q -> d -> IsRelevant
        lookupQrel defaultRel queryId docName =
            case queryId `M.lookup` qrelMap of
                Nothing -> defaultRel
                Just dMap -> case  docName `M.lookup` dMap of
                                Nothing -> defaultRel
                                Just r -> r

        totalRels :: M.Map q Int
        !totalRels = fmap countRel qrelMap
                    where countRel :: M.Map x IsRelevant -> Int
                          countRel m = length [ r
                                                | (_, r) <- M.toList m
                                                , QRel.isPositive r
                                                ]

        metric :: ScoringMetric IsRelevant q
        metric = meanAvgPrec (\q -> fromMaybe 0 $ q `M.lookup` totalRels)  Relevant



    return $ QrelInfo{qrelData = qrelData', qrelMap = qrelMap, lookupQrel = lookupQrel, totalRels = totalRels, metric = metric, metricName = "map"}


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


doPredict :: forall ph q d  . (Ord q, Ord d, Show q, Show d, Render q, Render d)
             => (SimplirRun.QueryId -> q) -> (SimplirRun.DocumentName -> d) 
            -> FeatureParams
            -> FilePath 
            -> Maybe DefaultFeatureParams
            -> Model Feat ph
            -> Maybe FilePath 
            -> IO () 
doPredict convQ convD featureParams@FeatureParams{..} outputFilePrefix defaultFeatureParamsOpt model qrelFileOpt  = do

    let fspace = modelFeatures model
        defaultFeatureVec = createDefaultFeatureVec fspace defaultFeatureParamsOpt
        FeatureSet {featureNames=_featureNames, produceFeatures=produceFeatures}
           = featureSet featureParams

    runFiles <- loadRunFiles convQ convD  featureRunsDirectory features
    putStrLn $ " loadRunFiles " <> (unwords $ fmap fst runFiles)

    
    QrelInfo{..} <- fromMaybe noQrelInfo 
                     <$> mapM (loadQrelInfo convQ convD) qrelFileOpt


    let featureDataMap = runFilesToFeatureVectorsMap fspace defaultFeatureVec produceFeatures runFiles
        featureDataList = fmap M.toList featureDataMap

        allDataList :: M.Map q [( d, FeatureVec Feat ph Double, Rel)]
        allDataList = augmentWithQrelsList_ (lookupQrel NotRelevant) featureDataList

        ranking = withStrategy (parTraversable rseq) 
                $ rerankRankings' model allDataList    

    case qrelFileOpt of
        Just _ -> storeRankingData outputFilePrefix ranking metric "predict"
        Nothing -> storeRankingDataNoMetric outputFilePrefix ranking "predict"


doTrain :: forall q d . (Ord q, Ord d, Show q, Show d, NFData q, NFData d, Aeson.FromJSON q, Aeson.FromJSON d, Render q, Render d)
            => (SimplirRun.QueryId -> q) -> (SimplirRun.DocumentName -> d) 
            -> FeatureParams
            -> FilePath 
            -> FilePath 
            -> FilePath 
            -> MiniBatchParams
            -> Bool
            -> Bool
            -> Bool
            -> ConvergenceDiagParams
            -> Maybe DefaultFeatureParams
            -> String
            -> IO ()
doTrain convQ convD featureParams@FeatureParams{..} outputFilePrefix experimentName qrelFile miniBatchParams includeCv useZScore saveHeldoutQueriesInModel convergenceParams defaultFeatureParamsOpt rankLipsVersion = do
    let FeatureSet {featureNames=featureNames,  produceFeatures=produceFeatures}
         = featureSet featureParams


    F.SomeFeatureSpace (fspace:: F.FeatureSpace Feat ph) <- pure $ F.mkFeatureSpace featureNames

    runFiles <- if featuresFromJsonL
                    then loadRunFiles convQ convD featureRunsDirectory features
                    else loadJsonLRunFiles featureRunsDirectory features
    putStrLn $ " loadRunFiles " <> (unwords $ fmap fst runFiles)

    QrelInfo{..} <- loadQrelInfo convQ convD qrelFile

    let defaultFeatureVec =  createDefaultFeatureVec fspace defaultFeatureParamsOpt

        featureDataMap = runFilesToFeatureVectorsMap  fspace defaultFeatureVec produceFeatures runFiles
        featureDataList :: M.Map q [( d, (F.FeatureVec Feat ph Double))] 
        featureDataList = fmap M.toList featureDataMap

        (featureDataList', createModelEnvelope') =
            if useZScore
                then
                    let zNorm :: Normalisation Feat ph Double
                        zNorm = zNormalizer $ [ feat
                                            | (_, list )<- M.toList featureDataList
                                            , (_, feat ) <- list
                                            ]
                        featureDataListZscore :: M.Map q [( d, (F.FeatureVec Feat ph Double))] 
                        featureDataListZscore = fmap normDocs featureDataList
                          where normDocs list =
                                    [ (doc, (normFeatures zNorm) feat)
                                    | (doc, feat) <- list    
                                    ]

                        modelConv (Model (WeightVec weights)) = Model (WeightVec $ denormWeights zNorm weights)

                    in (featureDataListZscore, createModelEnvelope modelConv)

                else (featureDataList, createModelEnvelope id)

        allDataListRaw :: M.Map q [( d, FeatureVec Feat ph Double, Rel)]
        allDataListRaw = augmentWithQrelsList_ (lookupQrel NotRelevant) featureDataList'

        allDataList = allDataListRaw


        modelEnvelope = createModelEnvelope' (Just experimentName) (Just miniBatchParams) (Just convergenceParams) (Just useZScore) (Just saveHeldoutQueriesInModel) (Just rankLipsVersion) defaultFeatureParamsOpt

    train includeCv fspace allDataList qrelData miniBatchParams convergenceParams  outputFilePrefix modelEnvelope


train :: forall ph q d . (Ord q, Ord d, Show q, Show d, NFData q, NFData d, Render q, Render d)
      =>  Bool
      -> F.FeatureSpace Feat ph
      -> TrainData Feat ph q d
      -> [QRel.Entry q d IsRelevant]
      -> MiniBatchParams
      -> ConvergenceDiagParams
      -> FilePath
      -> (Maybe Integer -> Maybe [q] -> Model Feat ph -> RankLipsModelSerialized Feat)
      -> IO()
train includeCv fspace allData qrel miniBatchParams convergenceDiagParams outputFilePrefix modelEnvelope =  do
    let metric :: ScoringMetric IsRelevant q
        !metric = meanAvgPrec (totalRelevantFromQRels qrel) Relevant
        totalElems = getSum . foldMap ( Sum . length ) $ allData
        totalPos = getSum . foldMap ( Sum . length . filter (\(_,_,rel) -> rel == Relevant)) $ allData

    putStrLn $ "Feature dimension: "++show (F.dimension $ F.featureSpace $ (\(_,a,_) -> a) $ head' $ snd $ M.elemAt 0 allData)
    putStrLn $ "Training model with (trainData) "++ show (M.size allData) ++
            " queries and "++ show totalElems ++" items total of which "++
            show totalPos ++" are positive."
    let displayTrainData :: Show f => TrainData f ph q d -> [String]
        displayTrainData trainData =
            [ show k ++ " " ++ show d ++ " " ++ show r ++ " -> "++ prettyFv
            | (k,list) <- M.toList trainData
            , (d,fvec, r) <- list
            , let prettyFv = unlines $ fmap show $ F.toList fvec
            ]

    putStrLn $ "Training Data = \n" ++ intercalate "\n" (take 10 $ displayTrainData $ force allData)
    gen0 <- newStdGen  -- needed by learning to rank
    trainMe includeCv miniBatchParams convergenceDiagParams
            gen0 allData fspace metric outputFilePrefix "" modelEnvelope




loadRunFiles :: forall q d 
             .  (SimplirRun.QueryId -> q) -> (SimplirRun.DocumentName -> d) 
             -> FilePath -> [FilePath] ->  IO [(FilePath, [SimplirRun.RankingEntry' q d])] 
loadRunFiles convQ convD prefix inputRuns = do
    fmap (second (map conv)) 
    <$> mapM (\fname -> (fname,)
    <$> SimplirRun.readRunFile (prefix </> fname)) inputRuns    
  where conv :: SimplirRun.RankingEntry -> SimplirRun.RankingEntry' q d    
        conv SimplirRun.RankingEntry{..} =
            SimplirRun.RankingEntry{ queryId= convQ queryId
                                    , documentName = convD documentName
                                    , .. }

newtype SerializedRankingEntry q d = SerializedRankingEntry { unserializeRankingEntry :: SimplirRun.RankingEntry' q d }
    deriving (Show, Generic)

instance (Aeson.FromJSON q, Aeson.FromJSON d) 
        => Aeson.FromJSON (SerializedRankingEntry q d) where
    parseJSON = Aeson.withObject "SerializedRankingEntry" $ \content -> do
        queryId <- content Aeson..: "query"
        documentName <- content Aeson..: "document"
        documentRank <- content Aeson..: "rank"    
        documentScore <- content Aeson..: "score"
        methodName <- fromMaybe "" <$> content Aeson..:? "method"      
        return $ SerializedRankingEntry (SimplirRun.RankingEntry{..})

instance (Aeson.ToJSON q, Aeson.ToJSON d) 
        => Aeson.ToJSON (SerializedRankingEntry q d) where
    toJSON (SerializedRankingEntry (SimplirRun.RankingEntry{..})) =
        Aeson.object $ [ "query" Aeson..= queryId
                       , "document" Aeson..= documentName
                       , "rank" Aeson..= documentRank
                       , "score" Aeson..= documentScore
                       ] <> case methodName of
                                "" -> []
                                name -> ["method" Aeson..= name]
    
loadJsonLRunFiles :: forall q d 
                  . (Aeson.FromJSON q, Aeson.FromJSON d)
                  => FilePath -> [FilePath] ->  IO [(FilePath, [SimplirRun.RankingEntry' q d])] 
loadJsonLRunFiles prefix inputRuns = do
    mapM (\fname -> (fname,)
        <$> readJsonLRunFile (prefix </> fname)) inputRuns    
  where readJsonLRunFile :: FilePath -> IO (([SimplirRun.RankingEntry' q d]))
        readJsonLRunFile fname = do
            bs <- BSL.readFile fname
            let decodeRankingEntry :: BSL.ByteString -> IO (SimplirRun.RankingEntry' q d)
                decodeRankingEntry bs = either fail (return . unserializeRankingEntry ) 
                                      $ Aeson.eitherDecode bs
            mapM decodeRankingEntry (BSL.lines bs)

writeJsonLRunFile :: forall q d
                  . (Aeson.ToJSON q, Aeson.ToJSON d)
                  => FilePath -> [SimplirRun.RankingEntry' q d] -> IO()
writeJsonLRunFile filename runEntries = do
    let lines :: [BSL.ByteString]
        lines = fmap (Aeson.encode . SerializedRankingEntry) $ runEntries
    BSL.writeFile filename $ BSL.unlines $ lines



augmentWithQrelsList_ ::  forall ph q d
                 . (q -> d -> Rel) 
                 -> M.Map q [( d, (F.FeatureVec Feat ph Double))] 
                 -> M.Map q [(d, F.FeatureVec Feat ph Double, Rel)]
augmentWithQrelsList_ lookupQrel qData =
    M.mapWithKey mapQueries qData
  where mapQueries :: q -> [(d, F.FeatureVec Feat ph Double)] -> [(d, F.FeatureVec Feat ph Double, Rel)]
        mapQueries query dMap =
            fmap mapDocs dMap
          where mapDocs :: (d,  F.FeatureVec Feat ph Double) -> (d,  F.FeatureVec Feat ph Double ,Rel)
                mapDocs (doc, feats) =
                    (doc, feats, lookupQrel query doc)


internFeatures :: F.FeatureSpace Feat ph -> [(Feat, d)] -> [(Feat, d)]
internFeatures fspace features =
    fmap internF features
  where 
    internF (f,v) =  
      let f' = F.internFeatureName fspace f
      in case f' of
            Just f'' -> (f'', v)
            Nothing -> error $ "Trying to intern feature "<> show f <> ", but is not defined in featurespace."


runFilesToFeatureVectorsMap :: forall ph q d . (Ord q, Ord d)
                          => F.FeatureSpace Feat ph 
                          -> F.FeatureVec Feat ph Double
                          -> (FilePath -> SimplirRun.RankingEntry' q d -> [(Feat, Double)])
                          -> [(FilePath, [SimplirRun.RankingEntry' q d])] 
                          -> M.Map q (M.Map d (F.FeatureVec Feat ph Double))
runFilesToFeatureVectorsMap fspace defaultFeatureVec produceFeatures runData = 
    let features:: M.Map q (M.Map d [(Feat, Double)]) 
        features = M.fromListWith (M.unionWith (<>))
                 $ [ ( queryId, 
                        M.fromListWith (<>)
                        [( documentName 
                         , internFeatures fspace $ produceFeatures fname entry
                        )]
                      )
                    | (fname, rankingEntries) <- runData
                    , entry@SimplirRun.RankingEntry {..} <- rankingEntries
                    ]

        featureVectors :: M.Map q (M.Map d (F.FeatureVec Feat ph Double))
        featureVectors =  fmap featureVectorize features           

    in featureVectors
  where featureVectorize :: M.Map d [(Feat, Double)] -> M.Map d (FeatureVec Feat ph Double)
        featureVectorize docFeatureList =
            fmap (F.modify defaultFeatureVec) docFeatureList

