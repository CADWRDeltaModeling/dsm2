#!/bin/bash
module load compiler/2022.0.2
module load mkl/2022.0.2
rm CMakeCache.txt
# cmake -DCMAKE_BUILD_TYPE=Debug -DJAVA_HOME=/home/knam/local/jdk/jdk1.6.0_45 -DCMAKE_CXX_COMPILER="icpc" -DCMAKE_C_COMPILER="icc" -DCMAKE_Fortran_COMPILER="ifort" -DTHIRD_PARTY_DIR=/home/knam/repos/dsm2_third_party_libs/builds/intel-2020.0.2/32bit/ -DCMAKE_Fortran_FLAGS="-fpp" -G "Unix Makefiles" ../src
cmake -DCMAKE_BUILD_TYPE=Release -DJAVA_HOME=/home/knam/local/jdk/jdk1.6.0_45 -DCMAKE_CXX_COMPILER="icpc" -DCMAKE_C_COMPILER="icc" -DCMAKE_Fortran_COMPILER="ifort" -DTHIRD_PARTY_DIR=/home/knam/repos/dsm2_third_party_libs/builds/intel-2022.0.2/64bit/ -DCMAKE_Fortran_FLAGS="-fpp" -G "Unix Makefiles" ../src
make -j10
