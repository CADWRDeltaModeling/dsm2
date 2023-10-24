@ECHO OFF
SETLOCAL
ECHO Start building DSM2...
cmake -E remove_directory build
cmake -E make_directory build
cd build
cmake -C "..\win_options.cmake" -G "Visual Studio 17 2022" -DCMAKE_BUILD_TYPE:STRING=Debug -A "x64" .. || goto :ERROR
cmake --build . --target ALL_BUILD --config Release -j %NUMBER_OF_PROCESSORS% || goto :ERROR
cpack -G "ZIP" -C Release || goto :ERROR
exit /B 0
:ERROR
exit /B 1
