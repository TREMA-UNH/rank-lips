#!/bin/bash


set -e
set -x



#bin="./"  # Path to the rank-lips binary

TREC_EVAL="trec_eval" # path to trec_eval binary (if not available, comment out section below)

OUT_DIR="./out"
OUT_PREFIX_TRAIN="train-try1"
OUT_PREFIX_TEST="predict-try1"
OUT_PREFIX_CV="cv-try1"
TRAIN_FEATURE_DIR="./train-features"
TRAIN_QREL="train.qrel"
TEST_FEATURE_DIR="./test-features"
TEST_QREL="test.qrel"
EXPERIMENT_NAME="my first rank-lips experiment"

FEAT_PARAM="--feature-variant FeatScore --z-score"
OPT_PARAM="--convergence-threshold 0.001 --mini-batch-size 1000  --folds 2 --restarts 10 --save-heldout-queries-in-model"



echo ""
echo " ---- TRAIN -----"


$bin/rank-lips train -d "${TRAIN_FEATURE_DIR}" -q "${TRAIN_QREL}" -e "${EXPERIMENT_NAME}" -O "${OUT_DIR}"  -o "${OUT_PREFIX_TRAIN}" ${OPT_PARAM} ${FEAT_PARAM}


ls ${OUT_DIR}

jq . < ${OUT_DIR}/${OUT_PREFIX_TRAIN}-model-train.json


echo ""
echo " ---- PREDICT ----- "

$bin/rank-lips predict -d "${TEST_FEATURE_DIR}" -q "${TEST_QREL}" -O "${OUT_DIR}"  -o "${OUT_PREFIX_TEST}" -m "${OUT_DIR}/${OUT_PREFIX_TRAIN}-model-train.json" ${FEAT_PARAM}



ls ${OUT_DIR} | grep "${OUT_PREFIX_TEST}"

cat "${OUT_DIR}/${OUT_PREFIX_TEST}-run-predict.run"

echo "For comparison: TREC Eval's evaluation"

# ${TREC_EVAL} -m map ${TEST_QREL} ${OUT_DIR}/${OUT_PREFIX_TEST}-run-predict.run



echo ""
echo " ---- CROSS-VALIDATION ----- "


$bin/rank-lips train --train-cv -d "${TRAIN_FEATURE_DIR}" -q "${TRAIN_QREL}" -e "${EXPERIMENT_NAME}" -O "${OUT_DIR}"  -o "${OUT_PREFIX_CV}" ${OPT_PARAM} ${FEAT_PARAM}


ls ${OUT_DIR} | grep "${OUT_PREFIX_CV}"


echo "Predicted Ranking"
cat ${OUT_DIR}/${OUT_PREFIX_CV}-run-test.run


echo "Train/Test MAP scores"

$bin/rank-lips train --train-cv -d "${TRAIN_FEATURE_DIR}" -q "${TRAIN_QREL}" -e "${EXPERIMENT_NAME}" -O "${OUT_DIR}"  -o "${OUT_PREFIX_CV}" ${OPT_PARAM} ${FEAT_PARAM} |& grep -e "Model test test metric" -e "Model train train metric"



echo "For comparison: TREC Eval's evaluation "

#${TREC_EVAL} -c -m map ${TRAIN_QREL} ${OUT_DIR}/${OUT_PREFIX_CV}-run-test.run


ls ${OUT_DIR} | grep ${OUT_PREFIX_CV}



echo ""
echo " ---- OTHER PARAMETERS ----- "


echo "enabling only features A and B"

OUT_PREFIX_TRAIN="train-try-feature-subset"
FEAT_PARAM="-f FeatureA -f FeatureB --feature-variant FeatScore --z-score"
OPT_PARAM="--convergence-threshold 0.001 --mini-batch-size 1000"

$bin/rank-lips train -d "${TRAIN_FEATURE_DIR}" -q "${TRAIN_QREL}" -e "${EXPERIMENT_NAME}" -O "${OUT_DIR}"  -o "${OUT_PREFIX_TRAIN}" ${OPT_PARAM} ${FEAT_PARAM}




echo "disabling z-score normalization"

OUT_PREFIX_TRAIN="train-try-no-zscore"
FEAT_PARAM="--feature-variant FeatScore" # removed "--z-score"
OPT_PARAM="--convergence-threshold 0.001 --mini-batch-size 1000 "

$bin/rank-lips train -d "${TRAIN_FEATURE_DIR}" -q "${TRAIN_QREL}" -e "${EXPERIMENT_NAME}" -O "${OUT_DIR}"  -o "${OUT_PREFIX_TRAIN}" ${OPT_PARAM} ${FEAT_PARAM}


echo "using both feature variations score and reciprocal rank"
OUT_PREFIX_TRAIN="train-try-feature-variants"
FEAT_PARAM="--feature-variant FeatScore --feature-variant FeatRecipRank --z-score"
OPT_PARAM="--convergence-threshold 0.001 --mini-batch-size 1000"

$bin/rank-lips train -d "${TRAIN_FEATURE_DIR}" -q "${TRAIN_QREL}" -e "${EXPERIMENT_NAME}" -O "${OUT_DIR}"  -o "${OUT_PREFIX_TRAIN}" ${OPT_PARAM} ${FEAT_PARAM}



echo "using tighter convergence parameter and smaller minibatches"
OUT_PREFIX_TRAIN="train-try-convergence-mini-batch"
FEAT_PARAM="--feature-variant FeatScore --feature-variant FeatRecipRank --z-score" 
OPT_PARAM="--convergence-threshold 0.0001 --mini-batch-size 1"  # we chose mini-batch of a single query, because we only have 2 train queries in total, typical minibatch-sizes are 10, 100, 1000

$bin/rank-lips train -d "${TRAIN_FEATURE_DIR}" -q "${TRAIN_QREL}" -e "${EXPERIMENT_NAME}" -O "${OUT_DIR}"  -o "${OUT_PREFIX_TRAIN}" ${OPT_PARAM} ${FEAT_PARAM}



