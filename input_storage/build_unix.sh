#!/bin/bash
#module load cmake
#module load intel/parallel_studio_2019/ia32
source ~/setup_intel_32.sh
#export PATH=~/localbin:$PATH
icc --version
ifort --version
FC=ifort
CC=icc
CXX=icpc
export FC
export CC
export CXX
cmake -E remove_directory BUILD
cmake -E make_directory BUILD
cd BUILD
cmake -G "Unix Makefiles" ../
#cmake --build . --target ALL_BUILD --config Debug
#cmake --build . --target ALL_BUILD --config Release
cmake --build .
