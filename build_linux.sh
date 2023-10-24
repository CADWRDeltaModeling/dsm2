#!/usr/bin/bash
cmake -E remove_directory build
cmake -E make_directory build
cd build
cmake -C ../linux_options.cmake -G "Unix Makefiles" \
    -DCMAKE_BUILD_TYPE:STRING=Release ..
cmake --build . --target all -j $(nproc)
cpack -G "TGZ"
