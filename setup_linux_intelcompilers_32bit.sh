module load cmake
module load intel/parallel_studio_2019/ia32
export PATH=~/localbin:$PATH
icc --version
ifort --version
export FC=ifort
export CC=icc
export CXX=icpc
export F9X=ifort
