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


module Main where

import Development.GitRev
import Control.Monad
import Data.Semigroup hiding (All, Any, option)
import Options.Applicative
import qualified Options.Applicative.Help.Pretty as Pretty
import System.FilePath
import Control.Concurrent (setNumCapabilities)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Maybe
import qualified Data.List.Split as Split
import System.Directory

import qualified SimplIR.Format.TrecRunFile as SimplirRun
import qualified SimplIR.Format.QRel as QRel
import SimplIR.LearningToRank


import RankLipsTypes
import EntFeatures
import SimplIR.Format.JsonRunQrels
import RankDataType


import Debug.Trace  as Debug
import qualified FeaturesAndSetup as RankLips

import qualified TrainAndSave as RankLips
import qualified Data.Aeson as Aeson






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


defaultConvergenceParams :: ConvergenceDiagParams
defaultConvergenceParams = ConvergenceDiagParams 10e-2 1000 0 (EvalCutoffAt 100) 5 5

convergenceParamParser :: Parser ConvergenceDiagParams
convergenceParamParser = 
     ConvergenceDiagParams
        <$> option auto (long "convergence-threshold" <> metavar "FACTOR" <> value defThresh  
                        <> help ("being converged means that relative change in MAP between iterations is less than FACTOR, default: "<> show defThresh))
        <*> option auto (long "convergence-max-iter" <> metavar "ITER" <> value defIter 
                        <> help ("max number of iterations after which training is stopped (use to avoid loops), default: "<> show defIter))
        <*> option auto (long "convergence-drop-initial-iterations" <> metavar "ITER" <> value defDrop 
                        <> help ("number of initial iterations to disregard before convergence is monitored, default: "<> show defDrop))
        <*> option (EvalCutoffAt <$> auto) (long "convergence-eval-cutoff" <> metavar "K"  <> value (EvalCutoffAt defEvalCutoff)
                        <> help ("Training MAP will only be evaluated on top K (saves runtime), default: "<> show defEvalCutoff))
        <*> option auto (short 'r' <> long "restarts" <> metavar "N" <> value defRestarts
                        <> help ("number of restarts per fold/model (model with best training performance will be chosen), default: "<> show defRestarts))
        <*> option auto (long "folds" <> metavar "K"  <> value defFolds
                        <> help ("number of folds (cross-validation only), default: "<> show defFolds))
  
  where ConvergenceDiagParams { convergenceThreshold=defThresh
                             , convergenceMaxIter=defIter
                             , convergenceDropInitIter=defDrop
                             , convergenceEvalCutoff=(EvalCutoffAt defEvalCutoff)
                             , convergenceRestarts=defRestarts
                             , convergenceFolds=defFolds
                             } = defaultConvergenceParams


featureParamsParser :: Parser FeatureParams
featureParamsParser = FeatureParams 
    <$> option str (long "feature-runs-directory" <> short 'd' <> help "directory containing run files for features" <> metavar "DIR")
    <*> many (option str (long "feature" <> short 'f' <> help "feature name, needs to match filename in feature-runs-directory" <> metavar "FEATURE") )
    <*> (some (option (parseFeatureVariant <$> str) (long "feature-variant" <> metavar "FVAR" 
            <> help ("Enable feature variant (default all), choices: " ++(show [minBound @FeatureVariant .. maxBound]) ))) 
        <|> pure  [minBound @FeatureVariant .. maxBound]    
        )
    <*> (flag' JsonLRunFormat  (long "jsonl" <> help "Load data from jsonl file instead of trec_eval run file")   
        <|> flag' JsonLGzRunFormat  (long "jsonl.gz" <> help "Load data from jsonl.gz file instead of trec_eval run file")   
        <|> flag' TrecEvalRunFormat  (long "trec-eval" <> help "Load data from trec_eval compatible run file")   
        <|> flag' TrecEvalRunFormat (help "Default: Load data from trec_eval compatible run file")
      )

defaultFeatureParamsParser :: Parser DefaultFeatureParams
defaultFeatureParamsParser = 
    ( DefaultFeatureSingleValue 
          <$> option auto (long "default-any-feature-value" <> metavar "VALUE" <> value 0.0
                          <> help "When any feature is missing for a query/doc pair, this value will be used as feature value (default: 0.0)." )
    ) <|>
    ( DefaultFeatureVariantValue
          <$> many ( option ( parseFeatureVariantPair =<< str ) (long "default-feature-variant-value" <> metavar "FVariant=VALUE" 
                          <> help "default values for each feature variant in FVariant=VALUE format without spaces, example: --default-feature-variant-value FeatureScore=-9999.999"))
    ) <|>
    ( DefaultFeatureValue
          <$> many ( option ( parseFeaturePair =<< str ) (long "default-feature-value" <> metavar "FNAME-FVariant=VALUE" 
                          <> help "default values for each feature in FNAME-FVariant=VALUE format without spaces, example: --default-feature-value FeatureA-FeatureScore=-9999.999"))
    )
  where parseFeatureVariantPair :: String -> ReadM (FeatureVariant, Double)
        parseFeatureVariantPair str =
            case Split.splitOn "=" str of
                [fv, val] ->  return $ (parseFeatureVariant fv, read val)
                _ -> fail $ "Ill-formed FVariant=VALUE format (expecting exactly one '='), got: "<> str
        parseFeaturePair :: String -> ReadM (FeatName, Double)
        parseFeaturePair str =
            case Split.splitOn "=" str of
                [fname, val] -> do
                    fname' <- parseFeatName (T.pack fname)
                    return (fname', read val)
                _ -> fail $ "Ill-formed FNAME-FVariant=VALUE format (expecting exactly one '='), got: "<> str





gitMsg :: String
gitMsg =  concat [ "[git ", $(gitBranch), "@", $(gitHash)
                 , " (", $(gitCommitDate), ")"
                 , "] " ]


getRankLipsVersion :: String
getRankLipsVersion = "ENT-Rank-lips version 1.1"

data ModelVersion = ModelVersionV10 | ModelVersionV11
    deriving (Eq, Read, Show)

opts :: Parser (IO ())
opts = subparser
    $  cmd "train"        doTrain'
    <>  cmd "conv-qrels"  doConvQrels'
    <>  cmd "conv-runs"   doConvRuns'
    <>  cmd "export-runs" doExportRuns'
    <>  cmd "rank-aggregation" doRankAggregation'
  where
    cmd name action = command name (info (helper <*> action) fullDesc)
     
    doPrintVersion =
        f <$> optional (option str (short 'v'))
      where 
          f :: Maybe String -> IO()
          f _v = putStrLn $ unlines [ getRankLipsVersion
                                    , "https://github.com/TREMA-UNH/rank-lips"
                                    -- , gitMsg
                                    ]
        
    doTrain' =
        f <$> featureParamsParser
          <*> option str (long "assocs" <> short 'a' <> metavar "JSONL" <> help "json file with associations between rank data fields")
          <*> option str (long "output-directory" <> short 'O' <> help "directory to write output to. (directories will be created)" <> metavar "OUTDIR")     
          <*> option str (long "output-prefix" <> short 'o' <> value "rank-lips" <> help "filename prefix for all written output; Default \"rank-lips\"" <> metavar "FILENAME")     
          <*> option str (long "qrels" <> short 'q' <> help "qrels file used for training" <> metavar "QRELS" )
          <*> option str (long "experiment" <> short 'e' <> help "experiment name (will be archived in the model file)" <> metavar "FRIENDLY_NAME" )
          <*> (minibatchParser <|> pure defaultMiniBatchParams)
          <*> (flag False True ( long "train-cv" <> help "Also train with 5-fold cross validation"))
          <*> (flag False True ( long "z-score" <> help "Z-score normalize features"))
          <*> (flag False True ( long "save-heldout-queries-in-model" <> help "Save heldout query ids in model file (cross-validation only)"))
          <*> convergenceParamParser
          <*> defaultFeatureParamsParser
          <*> option auto (short 'j' <> long "threads" <> help "enable multi-threading with J threads" <> metavar "J" <> value 1)
          <*> some (option ( RankDataField . T.pack <$> str) (short 'P' <> long "predict" <> metavar "FIELD" <> help "json field predict" ))
      where
        f :: FeatureParams -> FilePath ->  FilePath -> FilePath -> FilePath -> String -> MiniBatchParams 
          -> Bool -> Bool -> Bool -> ConvergenceDiagParams-> DefaultFeatureParams -> Int -> [RankDataField] -> IO()
        f fparams@FeatureParams{..} assocsFile outputDir outputPrefix qrelFile experimentName miniBatchParams 
          includeCv useZscore saveHeldoutQueriesInModel convergenceParams defaultFeatureParams numThreads predictField = do
            setNumCapabilities numThreads
            dirFeatureFiles <- listDirectory featureRunsDirectory
            createDirectoryIfMissing True outputDir
            let features' = case features of
                                [] -> dirFeatureFiles
                                fs -> fs 
                outputFilePrefix = outputDir </> outputPrefix

                predictFieldSet = S.fromList $ predictField
            doEntTrain @T.Text (fparams{features=features'}) assocsFile outputFilePrefix experimentName qrelFile miniBatchParams includeCv useZscore saveHeldoutQueriesInModel convergenceParams (Just defaultFeatureParams) predictFieldSet getRankLipsVersion
            
    doConvQrels' =
        f <$> option str (long "output" <> short 'o' <> help "location of new qrels file" <> metavar "FILE")     
          <*> option str (long "qrels" <> short 'q' <> help "qrels file used for training" <> metavar "QRELS" )
          <*> option ( RankDataField . T.pack <$> str) (short 'p' <> long "qrel-field" <> metavar "FIELD" <> help "json field represented in qrels file" )
      where
        f :: FilePath -> FilePath -> RankDataField ->  IO()
        f outputFile qrelFile qrelField = do
            -- createDirectoryIfMissing True outputFile

            qrels <- readTrecEvalQrelFile id convD qrelFile
            writeJsonLQrelFile outputFile qrels
          where  
            convD :: QRel.DocumentName -> RankData
            convD txt = singletonRankData qrelField (RankDataText txt)
            
    doConvRuns' =
        f <$> option str (long "output" <> short 'o' <> help "location of new run file" <> metavar "FILE")     
          <*> option str (long "run" <> short 'r' <> help "trec_eval run file " <> metavar "RUN" )
          <*> option ( RankDataField . T.pack <$> str) (short 'p' <> long "run-field" <> metavar "FIELD" <> help "json field representing data in file" )
      where
        f :: FilePath -> FilePath -> RankDataField ->  IO()
        f outputFile runFile runField = do
            runData <- readTrecEvalRunFile id convD runFile
            writeJsonLRunFile outputFile runData
          where  
            convD :: SimplirRun.DocumentName -> RankData
            convD txt = singletonRankData runField (RankDataText txt)
            
    doExportRuns' =
        f <$> option str (long "output" <> short 'o' <> help "location of new trec-eval run file" <> metavar "FILE")     
          <*> option str (long "run" <> short 'r' <> help "json run file " <> metavar "RUN" )
          <*> option ( RankDataField . T.pack <$> str) (short 'p' <> long "run-field" <> metavar "FIELD" <> help "json field to expose in run file" )
      where
        f :: FilePath -> FilePath -> RankDataField ->  IO()
        f outputFile runFile runField = do
            runData <- readJsonLRunFile runFile
            writeTrecEvalRunFile outputFile runData id convD
          where  
            convD :: RankData -> SimplirRun.DocumentName
            convD (RankData m) = fromMaybe (errMsg m) $ fmap unwrap $ runField `M.lookup` m
            errMsg m = error $ "Can't find field "<> show runField <>" in json object "<> show m
            unwrap (RankDataText t) = t
            unwrap (RankDataList l) = T.intercalate "," l

    doRankAggregation' =
        f <$> featureParamsParser
          -- <*> optional (option str (long "assocs" <> short 'a' <> metavar "JSONL" <> help "json file with associations between rank data fields"))
          <*> option str (long "output-directory" <> short 'O' <> help "directory to write output to. (directories will be created)" <> metavar "OUTDIR")     
          <*> option str (long "output-prefix" <> short 'o' <> value "rank-lips" <> help "filename prefix for all written output; Default \"rank-lips\"" <> metavar "FILENAME")     
          <*> option auto (short 'j' <> long "threads" <> help "enable multi-threading with J threads" <> metavar "J" <> value 1)
          <*> some (option ( RankDataField . T.pack <$> str) (short 'P' <> long "predict" <> metavar "FIELD" <> help "json field predict" ))
      where
        f :: --forall q d . (Aeson.ToJSON q, Aeson.FromJSON q, Aeson.ToJSON d, Aeson.FromJSON d, Ord q, Ord d, Eq q, Eq d)  =>
          FeatureParams -> FilePath -> FilePath -> Int -> [RankDataField] -> IO()
        f featureParams@FeatureParams{..} outputDir outputPrefix numThreads predictField = do
            setNumCapabilities numThreads
            dirFeatureFiles <- listDirectory featureRunsDirectory
            createDirectoryIfMissing True outputDir
            let features' = case features of
                                [] -> dirFeatureFiles
                                fs -> fs 
                outputFilePrefix = outputDir </> outputPrefix

                predictFieldSet = S.fromList $ predictField

                -- FeatureSet {featureNames=featureNames,  produceFeatures=produceFeatures}
                    -- = featureSet featureParams        
            runFiles <- RankLips.loadJsonLRunFiles featuresRunFormat featureRunsDirectory features
            putStrLn $ " loadRunFiles " <> (unwords $ fmap fst runFiles)

            let runData :: [(FilePath, [SimplirRun.RankingEntry' T.Text RankData])]             
                runData = runFiles
                aggrFeats = aggrFeatures runData
                aggrRun = [  SimplirRun.RankingEntry { queryId = query
                                                     , documentName = doc
                                                     , documentScore = score
                                                     , documentRank = 1
                                                     , methodName = "aggr"
                                                     }
                          | (query, map) <- M.toList aggrFeats
                          , (doc, score) <- M.toList map
                          ]
            writeJsonLRunFile (outputDir <> outputPrefix <.> "aggr"<.>"jsonl") $ aggrRun

        aggrFeatures :: forall q d . (Ord q, Ord d) =>  [(FilePath, [SimplirRun.RankingEntry' q d])]  -> M.Map q (M.Map d Double)
        aggrFeatures runData = 
                    M.fromListWith (M.unionWith (+))
                    $ [ ( queryId, M.singleton documentName  ( 1.0 / (realToFrac documentRank)) )
                      | (fname:: FilePath, rankingEntries:: [SimplirRun.RankingEntry' q d]) <- runData
                      , (entry@(SimplirRun.RankingEntry {..}) :: SimplirRun.RankingEntry' q d) <- rankingEntries
                      ]
    

debugTr :: (x -> String) ->  x -> x
debugTr msg x = Debug.trace (msg x) $ x


main :: IO ()
main = join $ execParser $ info (helper <*> opts) (progDescDoc (Just desc) <> fullDesc)
  where
    desc = Pretty.vcat $ Pretty.punctuate Pretty.linebreak
        [ para [ "ENT Rank-lips " ]
        ]
    para = Pretty.fillSep . map Pretty.text . foldMap words
  
