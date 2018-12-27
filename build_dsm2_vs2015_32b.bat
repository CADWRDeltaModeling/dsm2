setlocal
call "c:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\bin\compilervars.bat" ia32 vs2015
cmake -E remove_directory BUILD
cmake -E make_directory BUILD
cd BUILD
cmake -G "Visual Studio 14 2015" ..\src
REM cmake --build . --target ALL_BUILD --config Debug
cmake --build . --target ALL_BUILD --config Release