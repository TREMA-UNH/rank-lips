#!/bin/bash

set -e
set -x



#bin="./"  # Path to the rank-lips binary

OUT_DIR="./out"
OUT_PREFIX="try1"
TEST_FEATURE_DIR="./features-paragraph/test"
TEST_QREL="test.qrel"
MODEL="./train-small--paragraph--rank-lips.json"
EXPERIMENT_NAME="eal backwards compatibility test"

FEAT_PARAM="--feature-variant FeatScore" 
OPT_PARAM="--z-score --default-any-feature-value 0.0 --convergence-threshold 0.001 --mini-batch-size 1000  --folds 5 --restarts 5 --save-heldout-queries-in-model"



echo ""
echo " ---- PREDICT ----- "

$bin/rank-lips predict --is-v10-model -d "${TEST_FEATURE_DIR}" -q "${TEST_QREL}" -O "${OUT_DIR}"  -o "${OUT_PREFIX}" -m "${MODEL}" ${FEAT_PARAM}



ls ${OUT_DIR}

head "${OUT_DIR}/${OUT_PREFIX}-run-predict.run"



