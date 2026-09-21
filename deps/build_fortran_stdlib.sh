#!/bin/bash
if [[ ! -d "build" ]]; then
    mkdir build
fi
cd build

git clone -b v0.7.0 https://github.com/fortran-lang/stdlib.git
cd stdlib

PREFIX=../../fortran_stdlib-0.7.0
cmake -S . -B build -DCMAKE_Fortran_COMPILER=ifx -DCMAKE_INSTALL_PREFIX=$PREFIX -DCMAKE_BUILD_TYPE=Release
cmake --build build --target install --parallel

# Build dependencies
cmake --build build/_deps/test-driver-build --target install --parallel

