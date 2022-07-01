#!/usr/bin/bash
#source ../setup_linux_intelcompilers_32bit.sh
module purge
module use /opt/intel/oneapi/modulefiles
module load cmake
module load compiler/2022.0.2
cmake -E remove_directory BUILD
cmake -E make_directory BUILD
cd BUILD
cmake -DCMAKE_CXX_FLAGS="-fPIC" -DCMAKE_CXX_COMPILER="icpc" -DCMAKE_C_COMPILER="icc" -DCMAKE_Fortran_COMPILER="ifort" -DTHIRD_PARTY_DIR=/home/knam/repos/dsm2_third_party_libs/builds/intel-2022.0.2/64bit/ -G "Unix Makefiles" ..
cmake --build . -j 10