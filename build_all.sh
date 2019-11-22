#!/bin/bash 
source ./setup_linux_intelcompilers_32bit.sh

cd oprule
files="lib/parser/op_rule_tab.cpp lib/parser/op_rule.cpp"
for f in $files; do
  if [ ! -f $f ]; then
    touch $f
  fi
done
cmake -E remove_directory BUILD
cmake -E make_directory BUILD
cd BUILD
cmake -DTHIRD_PARTY_DIR=/usr/local/dsm2/third_party -G "Unix Makefiles" ../
make -j4
cd ../..

cd input_storage
cmake -E remove_directory BUILD
cmake -E make_directory BUILD
cd BUILD
cmake -DTHIRD_PARTY_DIR=/usr/local/dsm2/third_party -G "Unix Makefiles" ../
make -j4
cd ../..

cd dsm2
files="src/common/version.fi"
for f in $files; do
  if [ -f $f ]; then
    rm $f
  fi
done
cmake -E remove_directory BUILD
cmake -E make_directory BUILD
cd BUILD
cmake -DTHIRD_PARTY_DIR=/usr/local/dsm2/third_party -G "Unix Makefiles" ../src
make -j4
