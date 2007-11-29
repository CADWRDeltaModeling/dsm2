#! /usr/bin/sh -f
# get DSM2 version number
mawk 'BEGIN {sq=sprintf("%c",39)};/dsm2_version/ \
 {split($0,junk,sq);print junk[2]}' $1
