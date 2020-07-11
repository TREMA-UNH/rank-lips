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
import Control.Monad
import Data.Semigroup hiding (All, Any, option)
import Options.Applicative
import qualified Options.Applicative.Help.Pretty as Pretty
import System.FilePath
import Control.Concurrent (setNumCapabilities)

import qualified Data.Set as S
import qualified Data.Text as T
import Data.List
import Data.Maybe
import qualified Data.List.Split as Split
import System.Directory

import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified SimplIR.Format.TrecRunFile as SimplirRun
import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F


import RankLipsTypes
import RankLipsCompat
import FeaturesAndSetup
import SimplIR.Format.JsonRunQrels
import RankLipsFeatureUtils

-- import Debug.Trace  as Debug



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
    <*> flag True False  (long "jsonl-run" <> help "Load data from jsonl file instead of trec_eval run file")   

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
getRankLipsVersion = "Rank-lips version 1.2"

data ModelVersion = ModelVersionV10 | ModelVersionV11
    deriving (Eq, Read, Show)

opts :: Parser (IO ())
opts = subparser
    $  cmd "train"        doTrain'
    <> cmd "predict"        doPredict'
    <> cmd "convert-old-model"        doConvertModel'
    <> cmd "version" doPrintVersion
    <> cmd "convert-features" doConvertFeatures'
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
      where
        f :: FeatureParams ->  FilePath -> FilePath -> FilePath -> String -> MiniBatchParams -> Bool -> Bool -> Bool -> ConvergenceDiagParams-> DefaultFeatureParams -> Int -> IO()
        f fparams@FeatureParams{..} outputDir outputPrefix qrelFile experimentName miniBatchParams includeCv useZscore saveHeldoutQueriesInModel convergenceParams defaultFeatureParams numThreads = do
            setNumCapabilities numThreads
            dirFeatureFiles <- listDirectory featureRunsDirectory
            createDirectoryIfMissing True outputDir
            let features' = case features of
                                [] -> dirFeatureFiles
                                fs -> fs 
                outputFilePrefix = outputDir </> outputPrefix
            doTrain id id (fparams{features=features'}) outputFilePrefix experimentName qrelFile miniBatchParams includeCv useZscore saveHeldoutQueriesInModel convergenceParams (Just defaultFeatureParams) getRankLipsVersion
            

    doPredict' =
        f <$> featureParamsParser
          <*> option str (long "output-directory" <> short 'O' <> help "directory to write output to. (directories will be created)" <> metavar "OUTDIR")     
          <*> option str (long "output-prefix" <> short 'o' <> value "rank-lips" <> help "filename prefix for all written output; Default \"rank-lips\"" <> metavar "FILENAME")     
          <*> optional (option str (long "qrels" <> short 'q' <> help "qrels file, if provided, test MAP scores will be reported" <> metavar "QRELS" ))
          <*> option str (long "model" <> short 'm' <> help "file where model parameters will be read from " <> metavar "FILE" )
          <*> flag ModelVersionV11 ModelVersionV10 (long "is-v10-model" <> help "for loading V1.0 rank-lips models")
      where
        f :: FeatureParams ->  FilePath ->   FilePath -> Maybe FilePath -> FilePath -> ModelVersion ->  IO()
        f fparams@FeatureParams{..}  outputDir outputPrefix  qrelFileOpt modelFile modelVersion = do
            let backwardsCompatibleModelLoader =
                    case modelVersion of
                        ModelVersionV11 -> loadRankLipsModel modelFile
                        ModelVersionV10 -> loadRankLipsV10Model modelFile

            (SomeRankLipsModel (lipsModel :: RankLipsModel f ph)) <- deserializeRankLipsModel <$> backwardsCompatibleModelLoader

            let model ::  Model Feat ph
                model = trainedModel lipsModel
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

            createDirectoryIfMissing True outputDir

            let revertedModelFeatureFiles = nub $ mapMaybe extractFeatFiles modelFeatureFiles
                outputFilePrefix = outputDir </> outputPrefix

            doPredict id id (fparams{features = revertedModelFeatureFiles }) outputFilePrefix (defaultFeatureParams lipsModel) model qrelFileOpt
          where
              extractFeatFiles :: Feat -> Maybe FilePath
              extractFeatFiles (Feat FeatNameInputRun{..}) = Just fRunFile
              extractFeatFiles _ = Nothing

    doConvertModel' =
        convertOldModel
             <$> argument str (metavar "FILE" <> help "old model file")
             <*> option str (long "output" <> short 'o' <> metavar "OUT" <> help "file where new model will be written to")

    doConvertFeatures' =
        doConvertFeatures
             <$> option str (short 'd' <> long "input-dir" <> metavar "IN-DIR" <> help "directory of old feature files")
             <*> many (argument str ( metavar "FILE" <> help "filenames of feature files"))
             <*> option str (long "output" <> short 'o' <> metavar "OUT-DIR" <> help "directory where new features will be written to")
             <*> (flag' OldRunToJsonLMode (long "old-run-features-to-jsonl")
                 <|> flag' JsonLRunToTrecEval (long "jsonl-run-to-trec-eval")
                 )
             
data ConvertFeatureMode = OldRunToJsonLMode | JsonLRunToTrecEval
    deriving (Eq, Show)


convertOldModel :: FilePath -> FilePath -> IO()
convertOldModel oldModelFile newModelFile = do
    (SomeRankLipsModel (convertedLipsModel :: RankLipsModel f ph))  <- deserializeRankLipsModel <$> loadRankLipsV10Model oldModelFile

    let serializedRankLipsModel = serializeRankLipsModel 
                                $ convertedLipsModel {rankLipsVersion = Just $ getRankLipsVersion
                                                                             <> ". (Converted from V1.0 model \'"
                                                                            <> oldModelFile
                                                                            <> "\')"
                                                     }
    BSL.writeFile newModelFile $ Aeson.encode $ serializedRankLipsModel


doConvertFeatures :: FilePath ->  [FilePath] -> FilePath -> ConvertFeatureMode-> IO()
doConvertFeatures oldDir filenames  newDir OldRunToJsonLMode = do
    runs <- loadRunFiles id id oldDir filenames
    forM_ runs (\(fname, content) ->
            writeJsonLRunFile (newDir</>fname) content
        ) 
doConvertFeatures oldDir filenames  newDir JsonLRunToTrecEval = do
    runs <- loadJsonLRunFiles oldDir filenames
    forM_ runs (\(fname, content) ->
            SimplirRun.writeRunFile (newDir</>fname) content
        ) 

main :: IO ()
main = join $ execParser $ info (helper <*> opts) (progDescDoc (Just desc) <> fullDesc)
  where
    desc = Pretty.vcat $ Pretty.punctuate Pretty.linebreak
        [ para [ "Rank-lips is a high-performance multi-threaded list-wise learning-to-rank implementation that supports mini-batched learning." ]
        , para [ "Rank-lips is designed to work with trec_eval file formats for defining runs (run format) and relevance data (qrel format)."
               , "The features will be taken from the score and/or reciprocal rank of each input file. The filename of an input run"
               , "(in the directory) will be used as a feature name. If you want to train a model and predict on a different test set"
               , "make sure that the input runs for test features are using exactly the sane filename. We recommend to create"
               , "different directories for training and test sets." ]
        , para [ "For more information on invidiual commands call:" ]
        , Pretty.indent 4 "rank-lips COMMAND -h"
        , para [ " Also see website: http://www.cs.unh.edu/~dietz/rank-lips/" ]
        ]
    para = Pretty.fillSep . map Pretty.text . foldMap words
  