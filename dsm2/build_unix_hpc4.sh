#!/usr/bin/bash
#source ../setup_linux_intelcompilers_32bit.sh
module purge
module use /opt/intel/oneapi/modulefiles
module load cmake
module load compiler/2022.0.2
module load mkl/2022.0.2
source ~/miniconda3/etc/profile.d/conda.sh
conda activate f2py
which python
# cmake -E remove_directory BUILD
# cmake -E make_directory BUILD
cd BUILD
cmake -DCMAKE_BUILD_TYPE=Release -DJAVA_HOME=/home/knam/local/jdk/jdk1.6.0_45-64bit -DCMAKE_CXX_COMPILER="icpc" -DCMAKE_C_COMPILER="icc" -DCMAKE_Fortran_COMPILER="ifort" -DTHIRD_PARTY_DIR=/home/knam/repos/dsm2_third_party_libs/builds/intel-2022.0.2/64bit/ -DCMAKE_Fortran_FLAGS="-fpp" -G "Unix Makefiles" ../src
# cmake --build . -j 10 $1
make -j10