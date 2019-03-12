setlocal
call "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries_2019\windows\bin\compilervars.bat" ia32 vs2017
PATH="C:\Program Files (x86)\CMake\bin\";%PATH%
cmake -E remove_directory BUILD
cmake -E make_directory BUILD
cd BUILD
cmake -G "Visual Studio 15 2017" ..\src
rem cmake --build . --target ALL_BUILD --config Debug
cmake --build . --target ALL_BUILD --config Release