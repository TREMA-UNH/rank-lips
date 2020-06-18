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


module Main where

import Development.GitRev
import Control.DeepSeq hiding (rwhnf)
import Control.Monad
import Control.Parallel.Strategies
import Data.Semigroup hiding (All, Any, option)
import Options.Applicative
import Data.Aeson
import System.Random
import GHC.Stack
import System.FilePath

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.List
import Data.Maybe
import System.Directory

import qualified SimplIR.Format.TrecRunFile as SimplirRun
import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (FeatureVec)
import SimplIR.FeatureSpace.Normalise

import qualified SimplIR.Format.QRel as QRel

import TrainAndSave

-- import Debug.Trace  as Debug
import Data.Functor.Contravariant (Contravariant(contramap))

type NumResults = Int


-- newtype FeatureName = FeatureName { getFeatureName :: T.Text }
--                     deriving stock (Ord, Eq, Show, Read, Generic)
--                     deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey)



data FeatName = FeatNameInputRun { fRunFile :: FilePath
                                 , featureVariant:: FeatureVariant
                                 }
    deriving (Show, Ord, Eq)

encodeFeatureName :: FeatName -> String
encodeFeatureName FeatNameInputRun{..} = fRunFile <> "-" <> show featureVariant

instance ToJSON FeatName where
    toJSON featNameInputRun = toJSON $ encodeFeatureName featNameInputRun

instance ToJSONKey FeatName where
    toJSONKey = contramap encodeFeatureName toJSONKey

instance FromJSONKey FeatName where
    fromJSONKey = FromJSONKeyText parseFeatName

parseFeatName :: T.Text -> FeatName
parseFeatName str =
        head' $ mapMaybe matchEnd $ featureVariant'
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
    parseJSON (Data.Aeson.String str) = 
        return $ parseFeatName str


    parseJSON x = fail $ "Can only parse FeatName from string values, received "<> show x


newtype Feat = Feat { featureName :: FeatName}
    deriving stock (Eq, Ord)
    deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey )
instance Show Feat where
    show = show . featureName
-- instance Read Feat where
--     readPrec = fmap Feat readPrec

augmentFname :: FeatureVariant -> FilePath -> Feat
augmentFname featureVariant fname = Feat $ FeatNameInputRun fname featureVariant



data FeatureVariant = FeatScore | FeatRecipRank
    deriving (Eq, Show, Ord,  Read, Enum, Bounded)  

type RankEntry = SimplirRun.DocumentName


minibatchParser :: Parser MiniBatchParams
minibatchParser = MiniBatchParams
    <$> option auto (long "mini-batch-steps" <> metavar "STEPS" <> value defSteps 
                    <> help ("iterations per mini-batch, default: "<> show defSteps))
    <*> option auto (long "mini-batch-size" <> metavar "SIZE" <> value defBatchSize 
                    <> help ("number of mini-batch training queries, default: " <> show defBatchSize))
    <*> option auto (long "mini-batch-eval" <> metavar "EVAL" <> value defEvalSteps 
                    <> help ("number of mini-batches before next training evaluation, default: " <> show defEvalSteps))

 where MiniBatchParams { miniBatchParamsBatchSteps = defSteps
                        , miniBatchParamsBatchSize = defBatchSize
                        , miniBatchParamsEvalSteps = defEvalSteps
                        } = defaultMiniBatchParams

-- defaultConvergence info threshold maxIter dropIter

defaultConvergenceParams :: ConvergenceDiagParams
defaultConvergenceParams = ConvergenceDiagParams 10e-2 1000 0

convergenceParamParser :: Parser ConvergenceDiagParams
convergenceParamParser = 
     ConvergenceDiagParams
        <$> option auto (long "convergence-threshold" <> metavar "FACTOR" <> value defThresh  
                        <> help ("being converged means that relative change in MAP between iterations is less than FACTOR, default: "<> show defThresh))
        <*> option auto (long "convergence-max-iter" <> metavar "ITER" <> value defIter 
                        <> help ("max number of iterations after which training is stopped (use to avoid loops), default: "<> show defIter))
        <*> option auto (long "convergence-drop-initial-iterations" <> metavar "ITER" <> value defDrop 
                        <> help ("number of initial iterations to disregard before convergence is monitored, default: "<> show defDrop))
  
  where ConvergenceDiagParams { convergenceThreshold=defThresh
                             , convergenceMaxIter=defIter
                             , convergenceDropInitIter=defDrop} = defaultConvergenceParams


data FeatureParams = FeatureParams { featureRunsDirectory :: FilePath
                                   , features :: [FilePath]
                                   , featureVariants :: [FeatureVariant]
                                   }
    deriving (Eq, Show)

featureParamsParser :: Parser FeatureParams
featureParamsParser = FeatureParams 
    <$> option str (long "feature-runs-directory" <> short 'd' <> help "directory containing run files for features" <> metavar "DIR")
    <*> many (option str (long "feature" <> short 'f' <> help "feature name, needs to match filename in feature-runs-directory" <> metavar "FEATURE") )
    <*> (some (option auto (long "feature-variant" <> metavar "FVAR" 
            <> help ("Enable feature variant (default all), choices: " ++(show [minBound @FeatureVariant .. maxBound]) ))) 
        <|> pure  [minBound @FeatureVariant .. maxBound]    
        )
    -- <*> (option str (long "default-features-file")  <|> option double (long "default-feature-value" <> value 0.0))




data FeatureSet = FeatureSet { featureNames :: S.Set Feat
                             , produceFeatures :: FilePath -> SimplirRun.RankingEntry -> [(Feat, Double)]
                             }


convertFeatureNames :: [FeatureVariant] -> [FilePath] -> S.Set Feat
convertFeatureNames featureVariants features = 
    S.fromList $ [ augmentFname ft run 
                | run <-  features
                , ft <- featureVariants
                ]


featureSet :: FeatureParams -> FeatureSet
featureSet FeatureParams{..} =
    let
        featureNames :: S.Set Feat
        featureNames = convertFeatureNames featureVariants features 

        produceFeatures :: FilePath -> SimplirRun.RankingEntry -> [(Feat, Double)]
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



data QrelInfo = QrelInfo { qrelData :: [QRel.Entry SimplirRun.QueryId SimplirRun.DocumentName IsRelevant]
                         , qrelMap :: M.Map SimplirRun.QueryId (M.Map SimplirRun.DocumentName IsRelevant)
                         , lookupQrel :: IsRelevant -> SimplirRun.QueryId -> SimplirRun.DocumentName -> IsRelevant
                         , totalRels :: M.Map SimplirRun.QueryId Int
                         , metric :: ScoringMetric IsRelevant SimplirRun.QueryId
                         , metricName :: T.Text
                         }
noQrelInfo :: QrelInfo                         
noQrelInfo = QrelInfo { qrelData = []
                      , qrelMap = mempty
                      , lookupQrel = (\rel _q _d -> rel)
                      , totalRels = mempty
                      , metric = const 0.0
                      , metricName = "-"
                      }


deserializeRankLipsModel ::  Show RankLipsMetaField => RankLipsModelSerialized f -> SomeRankLipsModel f
deserializeRankLipsModel RankLipsModelSerialized{..} =
    case rankLipsTrainedModel of
      SomeModel trainedModel -> 
        let    rankLipsModel  = foldl readMeta (defaultRankLipsModel trainedModel) rankLipsMetaData
        in SomeRankLipsModel rankLipsModel
  where readMeta :: RankLipsModel f ph -> RankLipsMetaField -> RankLipsModel f ph
        readMeta rlm metafield =
          case metafield of
            RankLipsMiniBatch params -> rlm {minibatchParamsOpt = Just params}
            RankLipsEvalCutoff params -> rlm {evalCutoffOpt = Just params}
            RankLipsUseZScore flag -> rlm {useZscore = Just flag}
            RankLipsCVFold fold -> rlm {cvFold = Just fold}
            RankLipsIsFullTrain -> rlm {cvFold = Nothing}
            RankLipsExperimentName name -> rlm {experimentName = Just name}
            RankLipsHeldoutQueries queries -> rlm {heldoutQueries = Just queries}
            RankLipsConvergenceDiagParams params -> rlm {convergenceDiagParameters = Just params}
            x -> error $ "Don't know how to read metadata field "<> (show x)




createModelEnvelope :: (Model f ph -> Model f ph)
                    -> Maybe String
                    -> Maybe MiniBatchParams
                    -> Maybe EvalCutoff
                    -> Maybe ConvergenceDiagParams
                    -> Maybe Bool
                    -> (Maybe Integer -> Maybe [SimplirRun.QueryId] -> ModelEnvelope f ph)
createModelEnvelope modelConv experimentName minibatchParamsOpt evalCutoffOpt convergenceDiagParameters useZscore =
    (\cvFold heldOutQueries someModel' -> 
        let trainedModel = modelConv someModel'
            rankLipsTrainedModel = SomeModel trainedModel
            rankLipsMetaData = catMaybes [ fmap RankLipsMiniBatch minibatchParamsOpt
                                         , fmap RankLipsEvalCutoff evalCutoffOpt
                                         , fmap RankLipsConvergenceDiagParams convergenceDiagParameters
                                         , fmap RankLipsUseZScore useZscore
                                         , fmap RankLipsExperimentName experimentName
                                         , fmap RankLipsCVFold cvFold, if cvFold == Nothing then Just RankLipsIsFullTrain else Nothing
                                         , fmap RankLipsHeldoutQueries heldOutQueries
                                         ]
        in RankLipsModelSerialized{..}
    )



gitMsg :: String
gitMsg =  concat [ "[git ", $(gitBranch), "@", $(gitHash)
                 , " (", $(gitCommitDate), ")"
                 , "] " ]

opts :: Parser (IO ())
opts = subparser
    $  cmd "train"        doTrain'
    <> cmd "predict"        doPredict'
    <> cmd "convert-old-model"        doConvertModel'
    <> cmd "version" doPrintVersion
  where
    cmd name action = command name (info (helper <*> action) fullDesc)
    
    doPrintVersion =
        f <$> optional (option str (short 'v'))
      where 
          f :: Maybe String -> IO()
          f _v = putStrLn $ unlines ["Rank-lips version 1.1"
                                    , "github.com:bgamari/mediawiki-annotate.git"
                                    , gitMsg
                                    ]
        
    doTrain' =
        f <$> featureParamsParser
          <*> option str (long "output-directory" <> short 'O' <> help "directory to write output to. (directories will be created)" <> metavar "OUTDIR")     
          <*> option str (long "output-prefix" <> short 'o' <> value "rank-lips" <> help "filename prefix for all written output; Default \"rank-lips\"" <> metavar "FILENAME")     
          <*> option str (long "qrels" <> short 'q' <> help "qrels file used for training" <> metavar "QRELS" )
          <*> option str (long "experiment" <> short 'e' <> help "experiment name (will be archived in the model file)" <> metavar "FRIENDLY_NAME" )
          <*> (minibatchParser <|> pure defaultMiniBatchParams)
          <*> (flag False True ( long "train-cv" <> help "Also train with 5-fold cross validation"))
          <*> (flag False True ( long "z-score" <> help "Z-score normalize features"))
          <*> convergenceParamParser
      where
        f :: FeatureParams ->  FilePath -> FilePath -> FilePath -> String -> MiniBatchParams -> Bool -> Bool -> ConvergenceDiagParams-> IO()
        f fparams@FeatureParams{..} outputDir outputPrefix qrelFile experimentName miniBatchParams includeCv useZscore convergenceParams = do
            dirFeatureFiles <- listDirectory featureRunsDirectory
            createDirectoryIfMissing True outputDir
            let features' = case features of
                                [] -> dirFeatureFiles
                                fs -> fs 
                outputFilePrefix = outputDir </> outputPrefix
            doTrain (fparams{features=features'}) outputFilePrefix experimentName qrelFile miniBatchParams includeCv useZscore convergenceParams

    doPredict' =
        f <$> featureParamsParser
          <*> option str (long "output-prefix" <> short 'o' <> help "directory and file prefix to write output to." <> metavar "OUT")     
          <*> optional (option str (long "qrels" <> short 'q' <> help "qrels file, if provided, test MAP scores will be reported" <> metavar "QRELS" ))
          <*> option str (long "model" <> short 'm' <> help "file where model parameters will be read from " <> metavar "FILE" )
      where
        f :: FeatureParams ->  FilePath -> Maybe FilePath -> FilePath ->  IO()
        f fparams@FeatureParams{..} outputFilePrefix qrelFileOpt modelFile = do

            SomeRankLipsModel (lipsModel :: RankLipsModel f ph) <- deserializeRankLipsModel <$> loadRankLipsModel modelFile

            let model = trainedModel lipsModel
                fspace = modelFeatures model  
                modelFeatureFiles = F.featureNames fspace 

            dirFiles <- listDirectory featureRunsDirectory
            let dirFeatureSet = convertFeatureNames [minBound @FeatureVariant .. maxBound @FeatureVariant] dirFiles
                modelFeatureSet :: S.Set Feat
                modelFeatureSet = (S.fromList modelFeatureFiles)
                missingFeatures = modelFeatureSet `S.difference` dirFeatureSet
            when (not $ S.null $ missingFeatures)
                $ fail $ "Missing files for features (which are defined in model file): "
                             ++ show missingFeatures

            let revertedModelFeatureFiles =  mapMaybe extractFeatFiles modelFeatureFiles
            doPredict (fparams{features = revertedModelFeatureFiles }) outputFilePrefix model qrelFileOpt
          where
              extractFeatFiles :: Feat -> Maybe FilePath
              extractFeatFiles (Feat FeatNameInputRun{..}) = Just fRunFile
              extractFeatFiles _ = Nothing

    doConvertModel' =
        convertOldModel
             <$> argument str (metavar "FILE" <> help "old model file")
             <*> option str (long "output" <> short 'o' <> metavar "OUT" <> help "file where new model will be written to")

loadQrelInfo :: FilePath -> IO QrelInfo
loadQrelInfo qrelFile = do
    qrelData <- QRel.readQRel qrelFile
                :: IO [QRel.Entry SimplirRun.QueryId SimplirRun.DocumentName IsRelevant]

    let qrelMap :: M.Map SimplirRun.QueryId (M.Map SimplirRun.DocumentName IsRelevant)
        qrelMap = M.fromListWith (<>)
                [ (queryId, M.singleton documentName relevance)
                | QRel.Entry {..}  <- qrelData 
                ]

        lookupQrel :: IsRelevant -> SimplirRun.QueryId -> SimplirRun.DocumentName -> IsRelevant
        lookupQrel defaultRel queryId docName =
            case queryId `M.lookup` qrelMap of
                Nothing -> defaultRel
                Just dMap -> case  docName `M.lookup` dMap of
                                Nothing -> defaultRel
                                Just r -> r

        totalRels :: M.Map SimplirRun.QueryId Int
        !totalRels = fmap countRel qrelMap
                    where countRel :: M.Map x IsRelevant -> Int
                          countRel m = length [ r
                                                | (_, r) <- M.toList m
                                                , QRel.isPositive r
                                                ]

        metric :: ScoringMetric IsRelevant SimplirRun.QueryId
        metric = meanAvgPrec (\q -> fromMaybe 0 $ q `M.lookup` totalRels)  Relevant



    return $ QrelInfo{qrelData = qrelData, qrelMap = qrelMap, lookupQrel = lookupQrel, totalRels = totalRels, metric = metric, metricName = "map"}



doPredict :: forall ph 
            . FeatureParams
            -> FilePath 
            -> Model Feat ph
            -> Maybe FilePath 
            -> IO () 
doPredict featureParams@FeatureParams{..} outputFilePrefix model qrelFileOpt  = do
    let FeatureSet {featureNames=_featureNames, produceFeatures=produceFeatures}
         = featureSet featureParams

    let fspace = modelFeatures model

    runFiles <- loadRunFiles  featureRunsDirectory features
    putStrLn $ " loadRunFiles " <> (unwords $ fmap fst runFiles)

    
    QrelInfo{..} <- fromMaybe noQrelInfo 
             <$> mapM loadQrelInfo qrelFileOpt


    let featureDataMap = runFilesToFeatureVectorsMap fspace produceFeatures runFiles
        featureDataList = fmap M.toList featureDataMap

        allDataList :: M.Map SimplirRun.QueryId [( SimplirRun.DocumentName, FeatureVec Feat ph Double, Rel)]
        allDataList = augmentWithQrelsList_ (lookupQrel NotRelevant) featureDataList

        ranking = withStrategy (parTraversable rseq) 
                $ rerankRankings' model allDataList    

    case qrelFileOpt of
        Just _ -> storeRankingData outputFilePrefix ranking metric "predict"
        Nothing -> storeRankingDataNoMetric outputFilePrefix ranking "predict"


doTrain :: FeatureParams
            -> FilePath 
            -> FilePath 
            -> FilePath 
            -> MiniBatchParams
            -> Bool
            -> Bool
            -> ConvergenceDiagParams
            -> IO ()
doTrain featureParams@FeatureParams{..} outputFilePrefix experimentName qrelFile miniBatchParams includeCv useZScore convergenceParams = do
    let FeatureSet {featureNames=featureNames,  produceFeatures=produceFeatures}
         = featureSet featureParams


    F.SomeFeatureSpace (fspace:: F.FeatureSpace Feat ph) <- pure $ F.mkFeatureSpace featureNames

    runFiles <- loadRunFiles  featureRunsDirectory features
    putStrLn $ " loadRunFiles " <> (unwords $ fmap fst runFiles)

    QrelInfo{..} <- loadQrelInfo qrelFile

    let featureDataMap = runFilesToFeatureVectorsMap fspace produceFeatures runFiles
        featureDataList :: M.Map SimplirRun.QueryId [( SimplirRun.DocumentName, (F.FeatureVec Feat ph Double))] 
        featureDataList = fmap M.toList featureDataMap

        (featureDataList', createModelEnvelope') =
            if useZScore
                then
                    let -- Todo: save norm parameter, so we can use it during prediction    
                        zNorm :: Normalisation Feat ph Double
                        zNorm = zNormalizer $ [ feat
                                            | (_, list )<- M.toList featureDataList
                                            , (_, feat ) <- list
                                            ]
                        featureDataListZscore :: M.Map SimplirRun.QueryId [( SimplirRun.DocumentName, (F.FeatureVec Feat ph Double))] 
                        featureDataListZscore = fmap normDocs featureDataList
                          where normDocs list =
                                    [ (doc, (normFeatures zNorm) feat)
                                    | (doc, feat) <- list    
                                    ]

                        modelConv (Model (WeightVec weights)) = Model (WeightVec $ denormWeights zNorm weights)

                    in (featureDataListZscore, createModelEnvelope modelConv)

                else (featureDataList, createModelEnvelope id)

        allDataListRaw :: M.Map SimplirRun.QueryId [( SimplirRun.DocumentName, FeatureVec Feat ph Double, Rel)]
        allDataListRaw = augmentWithQrelsList_ (lookupQrel NotRelevant) featureDataList'

        -- remove queries without positive (and negative) training data
        allDataListTrainable = M.filter isTrainable allDataListRaw
          where isTrainable :: [( SimplirRun.DocumentName, FeatureVec Feat ph Double, Rel)] -> Bool
                isTrainable value =
                    let (vsFalse,vsTrue) = partition (\(doc,feat,rel)-> rel == NotRelevant)  value
                    in (not $ null vsFalse) && (not $ null $ vsTrue)

        allDataList = allDataListTrainable

        evalCutoff = (EvalCutoffAt 100)

        modelEnvelope = createModelEnvelope' (Just experimentName) (Just miniBatchParams) (Just evalCutoff) (Just convergenceParams) (Just useZScore)

    train includeCv fspace allDataList qrelData miniBatchParams convergenceParams evalCutoff  outputFilePrefix modelEnvelope


train :: Bool
      -> F.FeatureSpace Feat ph
      -> TrainData Feat ph
      -> [QRel.Entry SimplirRun.QueryId doc IsRelevant]
      -> MiniBatchParams
      -> ConvergenceDiagParams
      -> EvalCutoff
      -> FilePath
      -> (Maybe Integer -> Maybe [SimplirRun.QueryId] -> Model Feat ph -> RankLipsModelSerialized Feat)
      -> IO()
train includeCv fspace allData qrel miniBatchParams convergenceDiagParams evalCutoff outputFilePrefix modelEnvelope =  do
    let metric :: ScoringMetric IsRelevant SimplirRun.QueryId
        !metric = meanAvgPrec (totalRelevantFromQRels qrel) Relevant
        totalElems = getSum . foldMap ( Sum . length ) $ allData
        totalPos = getSum . foldMap ( Sum . length . filter (\(_,_,rel) -> rel == Relevant)) $ allData

    putStrLn $ "Feature dimension: "++show (F.dimension $ F.featureSpace $ (\(_,a,_) -> a) $ head' $ snd $ M.elemAt 0 allData)
    putStrLn $ "Training model with (trainData) "++ show (M.size allData) ++
            " queries and "++ show totalElems ++" items total of which "++
            show totalPos ++" are positive."
    let displayTrainData :: Show f => TrainData f ph -> [String]
        displayTrainData trainData =
            [ show k ++ " " ++ show d ++ " " ++ show r ++ " -> "++ prettyFv
            | (k,list) <- M.toList trainData
            , (d,fvec, r) <- list
            , let prettyFv = unlines $ fmap show $ F.toList fvec
            ]

    putStrLn $ "Training Data = \n" ++ intercalate "\n" (take 10 $ displayTrainData $ force allData)
    gen0 <- newStdGen  -- needed by learning to rank
    trainMe includeCv miniBatchParams convergenceDiagParams evalCutoff
            gen0 allData fspace metric outputFilePrefix "" modelEnvelope




loadRunFiles :: FilePath -> [FilePath] ->  IO [(FilePath, [SimplirRun.RankingEntry])] 
loadRunFiles prefix inputRuns = do
    mapM (\fname -> (fname,) <$> SimplirRun.readRunFile (prefix </> fname)) inputRuns    
                

augmentWithQrelsMap_ :: (SimplirRun.QueryId -> SimplirRun.DocumentName -> Rel) 
                 -> M.Map SimplirRun.QueryId (M.Map SimplirRun.DocumentName (F.FeatureVec Feat ph Double)) 
                 -> M.Map SimplirRun.QueryId (M.Map SimplirRun.DocumentName (F.FeatureVec Feat ph Double, Rel))
augmentWithQrelsMap_ lookupQrel qData =
    M.mapWithKey mapQueries qData
  where mapQueries :: SimplirRun.QueryId -> (M.Map SimplirRun.DocumentName d) -> M.Map SimplirRun.DocumentName (d, Rel)
        mapQueries query dMap =
            M.mapWithKey mapDocs dMap
          where mapDocs :: SimplirRun.DocumentName -> d -> (d,Rel)
                mapDocs doc feats =
                    (feats, lookupQrel query doc)


augmentWithQrelsList_ :: (SimplirRun.QueryId -> SimplirRun.DocumentName -> Rel) 
                 -> M.Map SimplirRun.QueryId [( SimplirRun.DocumentName, (F.FeatureVec Feat ph Double))] 
                 -> M.Map SimplirRun.QueryId [(SimplirRun.DocumentName, F.FeatureVec Feat ph Double, Rel)]
augmentWithQrelsList_ lookupQrel qData =
    M.mapWithKey mapQueries qData
  where mapQueries :: SimplirRun.QueryId -> [(SimplirRun.DocumentName, d)] -> [(SimplirRun.DocumentName, d, Rel)]
        mapQueries query dMap =
            fmap mapDocs dMap
          where mapDocs :: (SimplirRun.DocumentName, d) -> (SimplirRun.DocumentName, d,Rel)
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


runFilesToFeatureVectorsMap :: forall ph . F.FeatureSpace Feat ph 
                          -> (FilePath -> SimplirRun.RankingEntry -> [(Feat, Double)])
                          -> [(FilePath, [SimplirRun.RankingEntry])] 
                          -> M.Map SimplirRun.QueryId (M.Map SimplirRun.DocumentName (F.FeatureVec Feat ph Double))
runFilesToFeatureVectorsMap fspace produceFeatures runData = 
    let features:: M.Map SimplirRun.QueryId (M.Map SimplirRun.DocumentName [(Feat, Double)]) 
        features = M.fromListWith (M.unionWith (<>))
                 $ [ (queryId, 
                        M.fromListWith (<>)
                        [(documentName 
                         , internFeatures fspace $ produceFeatures fname entry
                        )]
                      )
                    | (fname, rankingEntries) <- runData
                    , entry@SimplirRun.RankingEntry {..} <- rankingEntries
                    ]
                    -- ToDo Where are default values handled?

        featureVectors :: M.Map SimplirRun.QueryId (M.Map SimplirRun.DocumentName (F.FeatureVec Feat ph Double))
        featureVectors =  fmap featureVectorize features           

    in featureVectors
  where featureVectorize :: M.Map SimplirRun.DocumentName [(Feat, Double)] -> M.Map SimplirRun.DocumentName (FeatureVec Feat ph Double)
        featureVectorize docFeatureList =
            fmap (F.fromList fspace)  docFeatureList






convertOldModel :: FilePath -> FilePath -> IO()
convertOldModel oldModelFile newRankLipsModelFile = do
    undefined

-- convertOldModel oldModelFile newRankLipsModelFile = do
--   SomeModel model <- loadOldModelData @Feat oldModelFile
--   let lipsModel = defaultRankLipsModel model
--   BSL.writeFile newRankLipsModelFile $ Data.Aeson.encode $ lipsModel




-- data RankingEntry = RankingEntry { queryId       :: !QueryId
--                                  , documentName  :: !DocumentName
--                                  , documentRank  :: !Rank
--                                  , documentScore :: !Score
--                                  , methodName    :: !MethodName
--                                  }
--                   deriving (Show)



head' :: HasCallStack => [a] -> a
head' (x:_) = x
head' [] = error $ "head': empty list"




main :: IO ()
main = join $ execParser $ info (helper <*> opts) mempty
