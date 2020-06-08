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

module Main where

import Control.Concurrent.Async
import Control.DeepSeq hiding (rwhnf)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad
import Control.Parallel.Strategies
import Control.Lens (each)
import Data.Coerce
import Data.Tuple
import Data.Semigroup hiding (All, Any, option)
import Options.Applicative
import System.IO
import Data.Aeson
import System.Random
import GHC.Generics
import GHC.Stack
import Control.Exception
import System.FilePath

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as ML
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Text.PrettyPrint.Leijen.Text as PP
import Data.List
import Data.Maybe
import Data.Foldable as Foldable
import Data.Hashable
import Control.Concurrent
import Control.Concurrent.Map
import Data.List.Split
import qualified Codec.Serialise as CBOR
import qualified SimplIR.Format.TrecRunFile as SimplirRun

import System.Directory



-- import CAR.Retrieve as Retrieve
-- import qualified CAR.RunFile as CarRun
-- import CAR.Utils
-- import GridFeatures

import qualified SimplIR.SimpleIndex as Index
import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (FeatureSpace, FeatureVec)
import SimplIR.FeatureSpace.Normalise
import SimplIR.Intern

import qualified SimplIR.Format.QRel as QRel
import qualified SimplIR.Ranking as Ranking

import Control.Monad


import TrainAndSave

import Debug.Trace  as Debug
import CAR.Types (ParagraphId)
import CAR.ToolVersion (execParser')

type NumResults = Int


newtype Feat = Feat { featureName :: T.Text }
    deriving (Eq, Show, Ord, Read)

data FeatureVariant = FeatScore | FeatRecipRank
    deriving (Eq, Show, Ord,  Read, Enum, Bounded)  


type RankEntry = SimplirRun.DocumentName




minibatchParser :: Parser MiniBatchParams
minibatchParser = MiniBatchParams
    <$> option auto (long "mini-batch-steps" <> metavar "STEPS" <> help "iterations per mini-batch")
    <*> option auto (long "mini-batch-size" <> metavar "SIZE" <> help "number of mini-batch training queries")
    <*> option auto (long "mini-batch-eval" <> metavar "EVAL" <> help "number of mini-batches before next training evaluation")


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




data FeatureSet = FeatureSet { featureNames :: S.Set Feat
                             , produceFeatures :: FilePath -> SimplirRun.RankingEntry -> [(Feat, Double)]
                             }


convertFeatureNames :: [FeatureVariant] -> [FilePath] -> S.Set Feat
convertFeatureNames featureVariants features = 
    S.fromList $ [ augmentFname ft run 
                | run <-  features
                , ft <- featureVariants
                ]

revertFeatureFileNames :: [Feat] -> [FilePath]
revertFeatureFileNames features =
    fmap revertFeat features
  where revertFeat :: Feat -> FilePath
        revertFeat (Feat name) =
            T.unpack $ T.dropEnd 1 $  T.dropWhileEnd(/= '-') name

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
                    ((augmentFname FeatScore fname), documentScore)
                produceFeature FeatRecipRank = 
                    ((augmentFname FeatRecipRank fname), (1.0/(realToFrac documentRank)))  
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
                      , lookupQrel = (\rel q d -> rel)
                      , totalRels = mempty
                      , metric = const 0.0
                      , metricName = "-"
                      }



opts :: Parser (IO ())
opts = subparser
    $  cmd "train"        doTrain'
    <> cmd "predict"        doPredict'
  where
    cmd name action = command name (info (helper <*> action) fullDesc)

    doTrain' =
        f <$> featureParamsParser
          <*> option str (long "output-prefix" <> short 'o' <> help "directory and file prefix to write output to." <> metavar "OUT")     
          <*> option str (long "qrels" <> short 'q' <> help "qrels file used for training" <> metavar "QRELS" )
          <*> option str (long "model" <> short 'm' <> help "file where model parameters will be written to" <> metavar "FILE" )
          <*> (minibatchParser <|> pure defaultMiniBatchParams)
          <*> (flag False True ( long "train-cv" <> help "Also train with 5-fold cross validation"))
     
      where
        f :: FeatureParams ->  FilePath -> FilePath -> FilePath -> MiniBatchParams -> Bool -> IO()
        f fparams@FeatureParams{..} outputFilePrefix qrelFile modelFile miniBatchParams includeCv = do
            dirFeatureFiles <- listDirectory featureRunsDirectory
            let features' = case features of
                                [] -> dirFeatureFiles
                                fs -> fs 
            doTrain (fparams{features=features'}) outputFilePrefix modelFile qrelFile miniBatchParams includeCv

    doPredict' =
        f <$> featureParamsParser
          <*> option str (long "output-prefix" <> short 'o' <> help "directory and file prefix to write output to." <> metavar "OUT")     
          <*> optional (option str (long "qrels" <> short 'q' <> help "qrels file used for training" <> metavar "QRELS" ))
          <*> option str (long "model" <> short 'm' <> help "file where model parameters will be written to" <> metavar "FILE" )
      where
        f :: FeatureParams ->  FilePath -> Maybe FilePath -> FilePath ->  IO()
        f fparams@FeatureParams{..} outputFilePrefix qrelFileOpt modelFile = do
            -- Todo: Load features from Model
            
            sm@(SomeModel model) <- loadModelData modelFile
                                    :: IO (SomeModel Feat)
            let fspace = modelFeatures model  
                modelFeatureFiles = F.featureNames fspace 

            dirFiles <- listDirectory featureRunsDirectory
            let dirFeatureSet = convertFeatureNames [minBound @FeatureVariant .. maxBound @FeatureVariant] dirFiles
                modelFeatureSet :: S.Set Feat
                modelFeatureSet = (S.fromList modelFeatureFiles)
                missingFeatures = modelFeatureSet `S.difference` dirFeatureSet
            when (not $ S.null $ missingFeatures)
                $ fail $ "Missing files for features (which are defined in model file): "
                             ++ show missingFeatures

            let revertedModelFeatureFiles = revertFeatureFileNames modelFeatureFiles
            doPredict (fparams{features= revertedModelFeatureFiles }) outputFilePrefix sm qrelFileOpt



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



doPredict :: FeatureParams
            -> FilePath 
            -> SomeModel Feat
            -> Maybe FilePath 
            -> IO () 
doPredict featureParams@FeatureParams{..} outputFilePrefix (SomeModel (model :: Model Feat ph)) qrelFileOpt  = do
    let FeatureSet {featureNames=featureNames, produceFeatures=produceFeatures}
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
            ->  IO ()
doTrain featureParams@FeatureParams{..} outputFilePrefix modelFile qrelFile miniBatchParams includeCv = do
    let FeatureSet {featureNames=featureNames,  produceFeatures=produceFeatures}
         = featureSet featureParams


    F.SomeFeatureSpace (fspace:: F.FeatureSpace Feat ph) <- pure $ F.mkFeatureSpace featureNames

    runFiles <- loadRunFiles  featureRunsDirectory features
    putStrLn $ " loadRunFiles " <> (unwords $ fmap fst runFiles)

    QrelInfo{..} <- loadQrelInfo qrelFile

    let featureDataMap = runFilesToFeatureVectorsMap fspace produceFeatures runFiles
        featureDataList = fmap M.toList featureDataMap

        allDataList :: M.Map SimplirRun.QueryId [( SimplirRun.DocumentName, FeatureVec Feat ph Double, Rel)]
        allDataList = augmentWithQrelsList_ (lookupQrel NotRelevant) featureDataList

    train includeCv fspace allDataList qrelData miniBatchParams outputFilePrefix modelFile


train :: Bool
      -> F.FeatureSpace Feat ph
      -> TrainData Feat ph
      -> [QRel.Entry SimplirRun.QueryId doc IsRelevant]
      -> MiniBatchParams
      -> FilePath
      -> FilePath
      -> IO()
train includeCv fspace allData qrel miniBatchParams outputFilePrefix modelFile =  do
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
    trainMe includeCv miniBatchParams (EvalCutoffAt 100) gen0 allData fspace metric outputFilePrefix modelFile



augmentFname :: FeatureVariant -> FilePath -> Feat
augmentFname featureVariant fname = Feat $ T.pack $ fname <> "-" <> show featureVariant

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
                         , produceFeatures fname entry
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
main = join $ execParser' 1 (helper <*> opts) mempty
