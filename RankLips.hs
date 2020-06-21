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
import qualified Options.Applicative.Help.Pretty as Pretty
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
import qualified Data.List.Split as Split
import System.Directory

import qualified SimplIR.Format.TrecRunFile as SimplirRun
import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (FeatureVec)
import SimplIR.FeatureSpace.Normalise

import qualified SimplIR.Format.QRel as QRel

import RankLipsTypes
import TrainAndSave
import FeaturesAndSetup

import Debug.Trace  as Debug

type NumResults = Int


-- newtype FeatureName = FeatureName { getFeatureName :: T.Text }
--                     deriving stock (Ord, Eq, Show, Read, Generic)
--                     deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey)




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
            RankLipsUseZScore flag -> rlm {useZscore = Just flag}
            RankLipsCVFold fold -> rlm {cvFold = Just fold}
            RankLipsIsFullTrain -> rlm {cvFold = Nothing}
            RankLipsExperimentName name -> rlm {experimentName = Just name}
            RankLipsHeldoutQueries queries -> rlm {heldoutQueries = Just queries}
            RankLipsConvergenceDiagParams params -> rlm {convergenceDiagParameters = Just params}
            RankLipsVersion version -> rlm {rankLipsVersion = Just version}
            RankLipsDefaultFeatures params -> rlm {defaultFeatureParams = Just params}
            x -> error $ "Don't know how to read metadata field "<> (show x)




gitMsg :: String
gitMsg =  concat [ "[git ", $(gitBranch), "@", $(gitHash)
                 , " (", $(gitCommitDate), ")"
                 , "] " ]


getRankLipsVersion :: String
getRankLipsVersion = "Rank-lips version 1.1"


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
      where
        f :: FeatureParams ->  FilePath -> FilePath -> FilePath -> String -> MiniBatchParams -> Bool -> Bool -> Bool -> ConvergenceDiagParams-> DefaultFeatureParams -> IO()
        f fparams@FeatureParams{..} outputDir outputPrefix qrelFile experimentName miniBatchParams includeCv useZscore saveHeldoutQueriesInModel convergenceParams defaultFeatureParams = do
            dirFeatureFiles <- listDirectory featureRunsDirectory
            createDirectoryIfMissing True outputDir
            let features' = case features of
                                [] -> dirFeatureFiles
                                fs -> fs 
                outputFilePrefix = outputDir </> outputPrefix
            doTrain (fparams{features=features'}) outputFilePrefix experimentName qrelFile miniBatchParams includeCv useZscore saveHeldoutQueriesInModel convergenceParams (Just defaultFeatureParams) getRankLipsVersion
            

    doPredict' =
        f <$> featureParamsParser
          <*> option str (long "output-directory" <> short 'O' <> help "directory to write output to. (directories will be created)" <> metavar "OUTDIR")     
          <*> option str (long "output-prefix" <> short 'o' <> value "rank-lips" <> help "filename prefix for all written output; Default \"rank-lips\"" <> metavar "FILENAME")     
          <*> optional (option str (long "qrels" <> short 'q' <> help "qrels file, if provided, test MAP scores will be reported" <> metavar "QRELS" ))
          <*> option str (long "model" <> short 'm' <> help "file where model parameters will be read from " <> metavar "FILE" )
      where
        f :: FeatureParams ->  FilePath ->  FilePath -> Maybe FilePath -> FilePath ->  IO()
        f fparams@FeatureParams{..}  outputDir outputPrefix  qrelFileOpt modelFile = do

            (SomeRankLipsModel (lipsModel :: RankLipsModel f ph)) <- deserializeRankLipsModel <$> loadRankLipsModel modelFile

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

            createDirectoryIfMissing True outputDir

            let revertedModelFeatureFiles = nub $ mapMaybe extractFeatFiles modelFeatureFiles
                outputFilePrefix = outputDir </> outputPrefix

            doPredict (fparams{features = revertedModelFeatureFiles }) outputFilePrefix (defaultFeatureParams lipsModel) model qrelFileOpt
          where
              extractFeatFiles :: Feat -> Maybe FilePath
              extractFeatFiles (Feat FeatNameInputRun{..}) = Just fRunFile
              extractFeatFiles _ = Nothing

    doConvertModel' =
        convertOldModel
             <$> argument str (metavar "FILE" <> help "old model file")
             <*> option str (long "output" <> short 'o' <> metavar "OUT" <> help "file where new model will be written to")






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
  