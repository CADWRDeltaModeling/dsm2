::Launches the script dsm2_tidy.py
::This script cleans up input files
@echo off

if "%1" == "--help" goto usage


if "%1" == "" goto do_dir

call vscript.bat %%~dp0\..\scripts\dsm2_tidy.py %1
goto end

:do_dir
if exist dsm2_tidy.zip del dsm2_tidy.zip
FOR %%i in (*.inp) do echo %%i
FOR %%i in (*.inp) do (
echo Processing %%i
call vscript.bat %%~dp0\..\scripts\dsm2_tidy.py %%i
)
goto end

:usage
echo Usage: dsm2_tidy [file]
echo If file is not given, all files in the directory will be tidied
echo.
echo. If you cannot verify your files run in DSM2
echo. you should back them up before you use this utility.
:end

