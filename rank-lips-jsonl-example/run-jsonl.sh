#! /bin/bash

set -x
set -e

mkdir -p train-features
mkdir -p test-features
$bin/rank-lips convert-features -d ../rank-lips-example/train-features FeatureA FeatureB FeatureC -o train-features
$bin/rank-lips convert-features -d ../rank-lips-example/test-features FeatureA FeatureB FeatureC -o test-features

$bin/rank-lips train -d "./train-features/" -q "train.qrel" -e "x" -O "out"  -o "x" --jsonl-run

cat out/x-run-train.run 

$bin/rank-lips predict -d "./test-features/" -q "test.qrel" -O "out"  -o "y" -m "./out/x-model-train.json" --jsonl-run
