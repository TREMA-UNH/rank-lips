#!/bin/bash -e

mkdir -p bin
for e in $(jq -r '..|."bin-file"?|strings' < dist-newstyle/cache/plan.json); do
    ln -fs $e bin/$(basename $e)
done
