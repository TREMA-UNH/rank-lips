#! /bin/bash

set -x
set -e

$bin/rank-lips convert-features -d ../rank-lips-example/train-features FeatureA FeatureB FeatureC -o train-features

$bin/rank-lips train -d "./train-features/" -q "train.qrel" -e "x" -O "out"  -o "x"

cat out/x-run-train.run 
