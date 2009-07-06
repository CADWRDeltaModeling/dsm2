::Launches the script dsm2_tidy.py
::This script cleans up input files
echo off
if "%1" == "" goto do_dir

call vscript.bat %%~dp0\..\scripts\dsm2_tidy.py %1 %1_tmp.inp
move /y %1_tmp.inp %1
goto end

:do_dir
FOR %%i in (*.inp) do echo %%i
FOR %%i in (*.inp) do (
echo Processing %%i
call vscript.bat %%~dp0\..\scripts\dsm2_tidy.py %%i %%i_tmp.inp
move /y %%i_tmp.inp %%i
)

:end

