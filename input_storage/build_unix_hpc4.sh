#!/usr/bin/bash
module purge
module load gcc/6.1.0
module load compiler/2022.1.0
module load cmake/3.21.1
cmake -E remove_directory build
cmake -E make_directory build
cd build
cmake -C ../../hpc4options.cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE:STRING=Release ..
cmake --build . --target all -j 10
cmake -C ../../hpc4options.cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE:STRING=Debug ..
cmake --build . --target all -j 10
