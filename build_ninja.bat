@ECHO OFF
SETLOCAL
REM CUSTOMIZE BELOW TO MATCH YOUR ENVIRONMENT
SET CYGWIN_DIR=D:\cygwin
SET PATH=%PATH%;%CYGWIN_DIR%\bin
@REM Activate a Python environment with fypp
CALL d:\venv\fortran\scripts\activate.bat
@REM Activate Intel oneAPI environment
CALL "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2022
ECHO Start building DSM2...
cmake -E remove_directory build
cmake -S . -B build -C ".\win_options.cmake" -G "Ninja" -DCMAKE_BUILD_TYPE=Release || goto :ERROR
cmake --build build --target all -j %NUMBER_OF_PROCESSORS% || goto :ERROR
cpack -G "ZIP" -C Release || goto :ERROR
exit /B 0
:ERROR
exit /B 1
