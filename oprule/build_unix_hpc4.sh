#!/usr/bin/bash
cmake -E remove_directory build
cmake -E make_directory build
cd build
cmake -C ../../hpc4options.cmake -G "Unix Makefiles" \
    -DCMAKE_BUILD_TYPE:STRING=Release ..
cmake --build . --target all -j 10
cmake -C ../../hpc4options.cmake -G "Unix Makefiles" \
    -DCMAKE_BUILD_TYPE:STRING=Debug ..
cmake --build . --target all -j 10
