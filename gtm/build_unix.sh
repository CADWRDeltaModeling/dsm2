#!/usr/bin/bash
source ../setup_linux_intelcompilers_32bit.sh
cmake remove_directory BUILD
cmake -E make_directory BUILD
cd BUILD
cmake -G "Unix Makefiles" ../src
#cmake --build . --target ALL_BUILD --config Debug
#cmake --build . --target ALL_BUILD --config Release
cmake --build .
