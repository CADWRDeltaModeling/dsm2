#!/bin/bash 
source ./setup_linux_intelcompilers_32bit.sh

cd gtm
cmake -E remove_directory BUILD
cmake -E make_directory BUILD
cd BUILD
cmake -DTHIRD_PARTY_DIR=/scratch/dms/dsm2/third_party -G "Unix Makefiles" ../src/
make -j4
cd ../../
