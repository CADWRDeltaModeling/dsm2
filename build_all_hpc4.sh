#!/bin/bash
source ./setup_linux_intelcompilers_32bit_hpc4.sh

cd oprule
#files="lib/parser/op_rule_tab.cpp lib/parser/op_rule.cpp"
#for f in $files; do
#  if [ ! -f $f ]; then
#    touch $f
#  fi
#done
cmake -E remove_directory BUILD
cmake -E make_directory BUILD
cd BUILD
cmake -DTHIRD_PARTY_DIR=/scratch/dms/dsm2/third_party -G "Unix Makefiles" ../
make clean
make -j4
cd ../..

cd input_storage
cmake -E remove_directory BUILD
cmake -E make_directory BUILD
cd BUILD
cmake -DTHIRD_PARTY_DIR=/scratch/dms/dsm2/third_party -G "Unix Makefiles" ../
make clean
make -j4
cd ../..

cd dsm2
#files="src/common/version.fi"
#for f in $files; do
#  if [ -f $f ]; then
#    rm $f
#  fi
#done
cmake -E remove_directory BUILD
cmake -E make_directory BUILD
cd BUILD
cmake -Dhydro_1000=on -DTHIRD_PARTY_DIR=/scratch/dms/dsm2/third_party -G "Unix Makefiles" ../src # -DCMAKE_BUILD_TYPE=Debug -DCMAKE_BUILD_TYPE=Debug
make clean
make -j4
