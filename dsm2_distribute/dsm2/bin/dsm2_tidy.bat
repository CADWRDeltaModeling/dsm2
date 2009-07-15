::Launches the script dsm2_tidy.py
::This script cleans up input files
@echo off

if "%1" == "--help" goto usage


if "%1" == "" goto do_dir

call vscript.bat %%~dp0\..\scripts\dsm2_tidy.py %1
goto end

:do_dir
FOR %%i in (*.inp) do echo %%i
FOR %%i in (*.inp) do (
echo Processing %%i
call vscript.bat %%~dp0\..\scripts\dsm2_tidy.py %%i
)
goto end

:usage
echo Usage: dsm2_tidy [file]
echo If file is not given, all files in the directory will be tidied

:end

