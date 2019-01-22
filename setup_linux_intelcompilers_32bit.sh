#!/bin/bash
module load cmake
source /opt/intel/parallel_studio_2019/compilers_and_libraries/linux/bin/compilervars.sh ia32
export PATH=~/localbin:$PATH
icc --version
ifort --version
export FC=ifort
export CC=icc
export CXX=icpc
export F9X=ifort
