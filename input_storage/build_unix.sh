#!/usr/bin/bash
# Script to build input storage on Linux
# It requires Intel Compilers are loaded already.

cmake -E remove_directory BUILD
cmake -E make_directory BUILD
cd BUILD
cmake -G "Unix Makefiles" ../
#cmake --build . --target ALL_BUILD --config Debug
#cmake --build . --target ALL_BUILD --config Release
cmake --build .
