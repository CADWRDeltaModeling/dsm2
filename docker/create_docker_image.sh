#!/bin/bash
ARTIFACTS="hydro qual"
for f in $ARTIFACTS; do
  cp ../dsm2/BUILD/$f .
done
docker build -t dsm2 .
for f in $ARTIFACTS; do
  rm $f
done
