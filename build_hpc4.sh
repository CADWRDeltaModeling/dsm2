#!/bin/bash
module purge
module use /opt/intel/oneapi/modulefiles
module load compiler/2022.1.0
module load cmake/3.21.1
cmake -E remove_directory build
cmake -E make_directory build
pushd build
cmake -C ../linux_options.cmake -G "Unix Makefiles" \
-DCMAKE_BUILD_TYPE:STRING=Release ..
cmake --build . --target all -j $(nproc)
cpack -G "TGZ"
popd
