#!/usr/bin/bash
cmake -E remove_directory build
cmake -E make_directory build
cd build
cmake -C ../../hpc4options.cmake -G "Unix Makefiles" \
-DCMAKE_BUILD_TYPE:STRING=Debug ../src
cmake --build . --target hydro_exe -j $(nproc)
