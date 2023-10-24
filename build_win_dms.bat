@ECHO OFF
SETLOCAL
REM CUSTOMIZE BELOW TO MATCH YOUR ENVIRONMENT
SET CYGWIN_DIR=D:\cygwin
SET PYTHON_DIR=C:\Users\foobar\Miniconda3\envs\dsm2
SET PATH=%PYTHON_DIR%;%CYGWIN_DIR%\bin;%PATH%
CALL C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2022
ECHO Start building DSM2...
cmake -E remove_directory build
cmake -E make_directory build
cd build
cmake -C "..\win_options.cmake" -G "Visual Studio 17 2022" -DCMAKE_BUILD_TYPE:STRING=Release -A "x64" .. || goto :ERROR
cmake --build . --target ALL_BUILD --config Release -j %NUMBER_OF_PROCESSORS% || goto :ERROR
cpack -G "ZIP" -C Release || goto :ERROR
exit /B 0
:ERROR
exit /B 1
