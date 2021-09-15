#!/bin/bash
#if OLD_LINUX (<=2.6) then uncomment line below and comment out other lines
#source /opt/intel15/composer_xe_2015.6.233/bin/compilervars.sh ia32
module load cmake
source /opt/intel/parallel_studio_xe_2019_update1/compilers_and_libraries/linux/bin/compilervars.sh ia32
export JAVA_HOME=/usr/bin/java
#export JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.232.b09-0.el7_7.i386
export PATH=~/localbin:$PATH
icc --version
ifort --version
export FC=ifort
export CC=icc
export CXX=icpc
export F9X=ifort

