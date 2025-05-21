#!/bin/bash
module purge
module load intel/2024.1
module load cmake/3.29.3
cmake -E remove_directory build
cmake -E make_directory build
pushd build
cmake -C ../linux_options.cmake -G "Unix Makefiles" \
-DCMAKE_BUILD_TYPE:STRING=Release ..
cmake --build . --target pydsm2gtm -j $(nproc)
cpack -G "TGZ"
popd
