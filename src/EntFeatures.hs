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
{-# LANGUAGE AllowAmbiguousTypes #-}

module EntFeatures where

import Control.DeepSeq hiding (rwhnf)
import Control.Parallel.Strategies
import Data.Semigroup hiding (All, Any, option)
import System.Random
import System.IO.Unsafe
import Data.IORef

import qualified Data.Map.Strict as M
import Data.List

import qualified Data.Aeson as Aeson
import Data.List.NonEmpty as NE

import qualified SimplIR.Format.TrecRunFile as SimplirRun
import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (FeatureVec)
import SimplIR.FeatureSpace.Normalise

import qualified SimplIR.Format.QRel as QRel

import RankLipsTypes
import qualified FeaturesAndSetup as RankLips
import qualified TrainAndSave as RankLips
import QrelInfo
import RankLipsFeatureUtils
import SimplIR.Format.JsonRunQrels
import RankDataType
import Control.Monad (when, void)
import GHC.Stack (HasCallStack)
import qualified Debug.Trace as Debug
import qualified Data.Text as T
import Data.Maybe
import qualified Data.Set as S
-- import Control.Concurrent.Map (mapConcurrentlyL)
import qualified SimplIR.Ranking as Ranking
import System.FilePath


scale :: Double -> [(Feat,Double)] ->  [(Feat,Double)] 
scale x feats = 
    [ (f, x * v) | (f, v) <- feats]



runFilesToEntFeatureVectorsMap :: forall ph q d . (Ord q, Ord d,      Show d, Show q)
                          =>  F.FeatureSpace Feat ph 
                          -> F.FeatureVec Feat ph Double
                          -> (q -> d -> [d])
                          -> (FilePath -> SimplirRun.RankingEntry' q d -> [(Feat, Double)])
                          -> [(FilePath, [SimplirRun.RankingEntry' q d])] 
                          -> M.Map q (M.Map d (F.FeatureVec Feat ph Double))
runFilesToEntFeatureVectorsMap fspace defaultFeatureVec resolveAssocs produceFeatures runData = 
    let features:: M.Map q (M.Map d [(Feat, Double)]) 
        features =
          projectFeatures
          $ producePlainFeatures runData

           
          where 
                -- | project out fields in features using associations
                projectFeatures :: M.Map q (M.Map d [(Feat,Double)]) -> M.Map q (M.Map d [(Feat,Double)])
                projectFeatures plainFeats = 
                    M.mapWithKey proj plainFeats
                  where proj queryId featMap =  
                                M.map (M.toList)  -- unwrap inner (M.Map Feat Double) to [(Feat,Double)]
                                $ M.fromListWith (M.unionWith (+))  -- M.Map d (M.Map Feat Double) to nub duplicate features with (+)
                                $ withStrategy (parBuffer 100 $ evalTuple2 rseq rseq)
                                --   $  [ Debug.trace ("projectFeatures: "<> unlines ["-queryId", show queryId,  "- documentName", show documentName, "- documentName'", show documentName'
                                --                                  --  , "- normalizationFactor", show normalizationFactor, "- featList", show featList])
                                $   [ ( documentName' 
                                        , M.fromListWith (+) 
                                          $ scale normalizationFactor featList
                                        )
                                    | (documentName, featList) <- M.toList featMap 
                                    , let !assocDocumentNames = resolveAssocs queryId documentName   --  <-- sloooow?
                                    , let !normalizationFactor = 1.0 / (realToFrac $ Data.List.length assocDocumentNames)
                                    , documentName' <- assocDocumentNames
                                    ]
                                -- Remarks: 
                                --    * documentName is the name associated with a feature from a runfile (e.g. neighbor/paragraph combination)
                                --    * documentName' is the name of the ranking we seek to produce (e.g. entity)


                -- | convert run files into list of features (keyed on q and d)
                producePlainFeatures :: [(FilePath, [SimplirRun.RankingEntry' q d])]  -> M.Map q (M.Map d [(Feat,Double)])
                producePlainFeatures runData = 
                    M.fromListWith (M.unionWith (++))
                          [ ( queryId
                            , M.singleton
                                documentName 
                                (internFeatures fspace $ produceFeatures fname entry)
                                :: M.Map d [(Feat, Double)]
                            )
                          | (fname:: FilePath, rankingEntries:: [SimplirRun.RankingEntry' q d]) <- runData
                          , (entry@(SimplirRun.RankingEntry {..}) :: SimplirRun.RankingEntry' q d) <- rankingEntries
                          ]

                 

        featureVectors :: M.Map q (M.Map d (F.FeatureVec Feat ph Double))
        featureVectors =  fmap featureVectorize features           

    in featureVectors
  where featureVectorize :: M.Map d [(Feat, Double)] -> M.Map d (FeatureVec Feat ph Double)
        featureVectorize docFeatureList =
            M.map (F.modify defaultFeatureVec) docFeatureList


createEntDefaultFeatureVec :: forall ph . F.FeatureSpace Feat ph ->  Maybe (DefaultFeatureParams) -> FeatureVec Feat ph Double
createEntDefaultFeatureVec fspace defaultFeatureParamsOpt =
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


memoize :: Ord k => (k -> v) -> (k -> v)
memoize f =
    unsafePerformIO $ do
        ref <- newIORef mempty
        return $ \x -> unsafePerformIO $ do
                            store <- readIORef ref
                            case x `M.lookup` store of
                                Just y -> return y
                                Nothing -> do 
                                    let !y = f x
                                    modifyIORef' ref (M.insert x y)  -- we read the ref again, because f might take a while to compute 
                                    return y




resolveAssociations :: (Eq q, Show q, Ord q) => S.Set RankDataField -> [SimplirRun.RankingEntry' q RankData] -> q -> RankData -> [RankData]
resolveAssociations predictFields assocs =
    let idx = M.fromListWith (<>)
            $   [ ((queryId, rdKey, rdValue), [documentName])
                | SimplirRun.RankingEntry {..}<- assocs
                , (rdKey, rdValue) <- rankDataAtoms documentName
                ] 
    in (\query doc ->
            let(x:xs) = [ S.fromList documentNames
                        | (partKey, partValue) <- rankDataAtoms doc 
                        , let documentNames = fromMaybe [] $ (query, partKey, partValue) `M.lookup` idx
                        ]
            in  fmap predictProj 
                $ S.toList 
                $ foldr (S.intersection) x xs            
       )

  where rankDataAtoms :: RankData -> [(RankDataField, RankDataValue)]
        rankDataAtoms (RankData m) =
                [(rdKey, rdValue)
                | (rdKey, rdValues) <- M.toList m
                , rdValue <- case rdValues of
                    txt@(RankDataText _) -> [txt]
                    RankDataList lst -> fmap RankDataText lst
                ] 
        predictProj :: RankData -> RankData
        predictProj rd =
            modRankData (\m -> M.filterWithKey (\k v -> k `S.member` predictFields) m) rd


resolveAssociationsOld :: (Eq q, Show q, Ord q) => S.Set RankDataField -> [SimplirRun.RankingEntry' q RankData] -> q -> RankData -> [RankData]
resolveAssociationsOld predictFields assocs =
    let assocIdx = M.fromListWith (<>)
                 $  [ (queryId, [documentName])
                        | SimplirRun.RankingEntry {..}<- assocs
                        ]
    in memoize $ \query doc ->
        let res =   [ predictProj documentName
                    | let rds = fromMaybe (error $ "no associations for query "<> (show query) <> " in assocIdx "<> (show assocIdx))
                            $ query `M.lookup` assocIdx
                    , documentName <-  rds
                    , partialMatch doc documentName
                    ]
        in res
        -- in if (null res)
            --    then Debug.trace ("No assocs for query "<> show query<> ", document "<> show doc) $ res        
            --    else res
            -- --    else Debug.trace ("Found Assoc query "<> show query<> ", document "<> show doc) $ res

  where partialMatch :: RankData -> RankData -> Bool
        partialMatch (RankData part) (RankData whole) =
            -- debugTr ("partialMatch part: "<> displayMap part <> "\n  whole "<> displayMap whole) $
            M.isSubmapOfBy equalsOrContains ( M.filter notNullList part) 
            $ M.filter notNullList whole
        notNullList :: RankDataValue -> Bool
        notNullList (RankDataText _) = True
        notNullList (RankDataList lst) = not $ null lst

        displayMap :: M.Map RankDataField RankDataValue -> String
        displayMap m =
            T.unpack $ T.unlines [ "("<> k <> ": "<> display v <> ")"| (RankDataField k,v) <- M.toList m]
    
    
        predictProj :: RankData -> RankData
        predictProj rd =
            modRankData (\m -> M.filterWithKey (\k v -> k `S.member` predictFields) m) rd


debugTr x y = Debug.trace (x <> show y) y



doEntPredict :: forall q ph  . (Ord q, Show q, Render q, Aeson.FromJSON q, Aeson.ToJSON q, Aeson.ToJSON IsRelevant)
            => FeatureParams
            -> FilePath
            -> FilePath 
            -> Maybe DefaultFeatureParams
            -> Model Feat ph
            -> Maybe FilePath 
            -> S.Set RankDataField
            -> IO () 
doEntPredict featureParams@FeatureParams{..} assocsFile outputFile defaultFeatureParamsOpt model qrelFileOpt predictionFields = do
    let fspace = modelFeatures model
        defaultFeatureVec = createDefaultFeatureVec fspace defaultFeatureParamsOpt
        FeatureSet {featureNames=featureNames,  produceFeatures=produceFeatures}
          = {-# SCC "featureSet" #-} featureSet featureParams

    -- QrelInfo{..} <- case qrelFileOpt of
    --                     Just qrelFile -> do
    --                         loadQrelInfo <$> readTrecEvalQrelFile convQ convD qrelFile
    --                     Nothing -> return $ noQrelInfo


    let FeatureSet {featureNames=featureNames,  produceFeatures=produceFeatures}
         = {-# SCC "featureSet" #-} featureSet featureParams

    putStrLn "ent-rank-lips starting run/assocs/qrel loading..."

    -- F.SomeFeatureSpace (fspace:: F.FeatureSpace Feat ph) <- pure $ F.mkFeatureSpace featureNames

    runFiles <- {-# SCC "loadJsonLRunFiles" #-} RankLips.loadJsonLRunFiles featuresRunFormat featureRunsDirectory features
   
    putStrLn $ " loadedRunFiles " <> (unwords $ fmap fst runFiles)

    assocs <- 
      {-# SCC "assocs" #-}
      if ("jsonl" `isSuffixOf` assocsFile)  
                    then readJsonLRunFile assocsFile
                    else if ("jsonl.gz" `isSuffixOf` assocsFile)  
                        then readGzJsonLRunFile assocsFile
                        else error $ "First convert file to jsonl or jsonl.gz "<> assocsFile


    when (null assocs) (fail $ "no associations found in "<> assocsFile)
    putStrLn $ " loaded Assoc File " <> (show $ Data.List.head assocs)

    let qrelFile = fromJust qrelFileOpt
    let projectGroundTruth = id
    QrelInfo{..} <- {-# SCC loadQrelInfo #-} (loadQrelInfo . projectGroundTruth 
                 <$>  if ("jsonl" `isSuffixOf` qrelFile)  
                        then readJsonLQrelFile qrelFile
                        else if ("jsonl.gz" `isSuffixOf` qrelFile)  
                            then readGzJsonLQrelFile qrelFile
                            else error $ "First convert file to jsonl or jsonl.gz "<> qrelFile)
                    
    putStrLn $ " loaded qrels " <> (unlines $ fmap show  $ Data.List.take 10 $ qrelData)

    putStrLn "ent-rank-lips starting feature creation..."

    let defaultFeatureVec =  createEntDefaultFeatureVec fspace defaultFeatureParamsOpt

        featureDataMap = {-# SCC "featureDataMap" #-} runFilesToEntFeatureVectorsMap fspace defaultFeatureVec (resolveAssociations predictionFields assocs) produceFeatures runFiles
        featureDataList :: M.Map q [( RankData, (F.FeatureVec Feat ph Double))] 
        featureDataList = fmap M.toList featureDataMap

        allDataListRaw :: M.Map q [( RankData, FeatureVec Feat ph Double, Rel)]
        allDataListRaw = RankLips.augmentWithQrelsList_ (lookupQrel QRel.NotRelevant) featureDataList



        ranking = withStrategy (parTraversable rseq)
                $ rerankRankings' model allDataListRaw    

        outRun = [ SimplirRun.RankingEntry {methodName ="predict", ..} 
                 | (queryId, rank) <- M.toList ranking
                 ,(documentRank, (documentScore, (documentName, _rel))) <- Data.List.zip [1..] $ Ranking.toSortedList rank
                 ]        

    if  ("jsonl.gz" `isSuffixOf` outputFile )
        then writeGzJsonLRunFile (outputFile) outRun
        else if ("jsonl" `isSuffixOf` outputFile )
            then writeJsonLRunFile (outputFile) outRun
            else writeGzJsonLRunFile (outputFile <.> "jsonl.gz") outRun
    
        
    let
        metric' :: ScoringMetric IsRelevant q
        metric' = metric 
        testScore = metric' ranking

    putStrLn $ "testScore "<> show testScore



doEntTrain :: forall q . (Ord q, Show q, NFData q,  Aeson.FromJSON q, Render q)
            => FeatureParams
            -> FilePath
            -> FilePath 
            -> FilePath 
            -> FilePath 
            -> MiniBatchParams
            -> Bool
            -> Bool
            -> Bool
            -> ConvergenceDiagParams
            -> Maybe DefaultFeatureParams
            -> S.Set RankDataField
            -> String
            -> IO ()
doEntTrain featureParams@FeatureParams{..} assocsFile outputFilePrefix experimentName qrelFile miniBatchParams 
           includeCv useZScore saveHeldoutQueriesInModel convergenceParams defaultFeatureParamsOpt predictionFields rankLipsVersion  = do
    putStrLn "ent-rank-lips making feature sets...."
    
    let FeatureSet {featureNames=featureNames,  produceFeatures=produceFeatures}
         = {-# SCC "featureSet" #-} featureSet featureParams

    putStrLn "ent-rank-lips starting run/assocs/qrel loading..."

    F.SomeFeatureSpace (fspace:: F.FeatureSpace Feat ph) <- pure $ F.mkFeatureSpace featureNames

    runFiles <- {-# SCC "loadJsonLRunFiles" #-} RankLips.loadJsonLRunFiles featuresRunFormat featureRunsDirectory features
   
    putStrLn $ " loadedRunFiles " <> (unwords $ fmap fst runFiles)

    assocs <- 
      {-# SCC "assocs" #-}
      if ("jsonl" `isSuffixOf` assocsFile)  
                    then readJsonLRunFile assocsFile
                    else if ("jsonl.gz" `isSuffixOf` assocsFile)  
                        then readGzJsonLRunFile assocsFile
                        else error $ "First convert file to jsonl or jsonl.gz "<> assocsFile


    when (null assocs) (fail $ "no associations found in "<> assocsFile)
    putStrLn $ " loaded Assoc File " <> (show $ Data.List.head assocs)

    let projectGroundTruth = id
    QrelInfo{..} <- {-# SCC loadQrelInfo #-} (loadQrelInfo . projectGroundTruth 
                 <$>  if ("jsonl" `isSuffixOf` qrelFile)  
                        then readJsonLQrelFile qrelFile
                        else if ("jsonl.gz" `isSuffixOf` qrelFile)  
                            then readGzJsonLQrelFile qrelFile
                            else error $ "First convert file to jsonl or jsonl.gz "<> qrelFile)
                    
    putStrLn $ " loaded qrels " <> (unlines $ fmap show  $ Data.List.take 10 $ qrelData)

    putStrLn "ent-rank-lips starting feature creation..."

    let defaultFeatureVec =  createEntDefaultFeatureVec fspace defaultFeatureParamsOpt

        featureDataMap = {-# SCC "featureDataMap" #-} runFilesToEntFeatureVectorsMap fspace defaultFeatureVec (resolveAssociations predictionFields assocs) produceFeatures runFiles
        featureDataList :: M.Map q [( RankData, (F.FeatureVec Feat ph Double))] 
        featureDataList = fmap M.toList featureDataMap

    putStrLn $ "entFeatureVectors" <> (unlines $ fmap show $ Data.List.take 1 $ M.toList $ M.map (Data.List.take 10) featureDataList )
    let
        (featureDataList', createModelEnvelope') =
            if useZScore
                then
                    let zNorm :: Normalisation Feat ph Double
                        zNorm = zNormalizer $ [ feat
                                            | (_, list )<- M.toList featureDataList
                                            , (_, feat ) <- list
                                            ]
                        featureDataListZscore :: M.Map q [( RankData, (F.FeatureVec Feat ph Double))] 
                        featureDataListZscore = fmap normDocs featureDataList
                          where normDocs list =
                                    [ (doc, (normFeatures zNorm) feat)
                                    | (doc, feat) <- list    
                                    ]

                        modelConv (Model (WeightVec weights)) = Model (WeightVec $ denormWeights zNorm weights)

                    in (featureDataListZscore, RankLips.createModelEnvelope modelConv)

                else (featureDataList, RankLips.createModelEnvelope id)


    putStrLn $ "featureDataList' " <> show featureDataList'
    let
        allDataListRaw :: M.Map q [( RankData, FeatureVec Feat ph Double, Rel)]
        allDataListRaw = RankLips.augmentWithQrelsList_ (lookupQrel QRel.NotRelevant) featureDataList'

        allDataList = allDataListRaw

        modelEnvelope = createModelEnvelope' (Just experimentName) (Just miniBatchParams) (Just convergenceParams) (Just useZScore) (Just saveHeldoutQueriesInModel) (Just rankLipsVersion) defaultFeatureParamsOpt

    putStrLn $ "Sample of allDataList \n" <> ( unlines $ fmap show $ Data.List.take 10 $ M.toList allDataList)
    putStrLn "ent-rank-lips starting training ..."

    train includeCv fspace allDataList qrelData miniBatchParams convergenceParams  outputFilePrefix modelEnvelope




train :: forall ph q d . (Ord q, Ord d, Show q, Show d, NFData q, NFData d, Render q, Render d, HasCallStack)
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
        totalElems = getSum . foldMap ( Sum . Data.List.length ) $ allData
        totalPos = getSum . foldMap ( Sum . Data.List.length . Data.List.filter (\(_,_,rel) -> rel == Relevant)) $ allData

    when (M.null allData) (fail "No features could be constructed")
    when (null $ snd $ M.elemAt 0 allData) (fail "First query does not have any features")


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

    putStrLn $ "Training Data = \n" ++ intercalate "\n" (Data.List.take 10 $ displayTrainData $ force allData)
    gen0 <- newStdGen  -- needed by learning to rank

    let experimentName = ""

    putStrLn "ready to optimize..."
    let (fullRestartResults, foldRestartResults) =
            RankLips.trainMe miniBatchParams convergenceDiagParams gen0 allData fspace metric

    let savedTrainedResult model = do
            RankLips.storeModelAndRanking outputFilePrefix experimentName modelEnvelope model
            return model

        (cvComputation, fullComputation) = withStrategy (parTuple2 (parTraversable rseq) rseq)
                                          $ RankLips.mapCvFull RankLips.bestRestart (foldRestartResults, fullRestartResults)

    if includeCv 
    then do
        (cvModels, fullModels) <- RankLips.mapIOCvFull savedTrainedResult (cvComputation, fullComputation)
        let testRanking = RankLips.computeTestRanking  $ cvModels
        void $ savedTrainedResult testRanking
    else do
        void $ savedTrainedResult fullComputation

    return ()





projectFeatureSpace :: forall q d d' f ph . (Ord f, Ord d')
                    => (d -> d')
                    -> M.Map q [( d, FeatureVec f ph Double)] 
                    -> M.Map q [( d', FeatureVec f ph Double)] 
projectFeatureSpace proj allDataList =
    let allDataListProjected = fmap projectList allDataList
    in allDataListProjected
  where projectList :: [(d, FeatureVec f ph Double)] -> [(d', FeatureVec f ph Double)]    
        projectList features =
            M.toList
            $ fmap (F.aggregateWith (+))
            $ M.fromListWith (<>)
            $ [ (proj doc, NE.fromList [feat]) | (doc, feat) <- features]

