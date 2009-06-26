echo off
echo processing
FOR %%i in (*.inp) do echo %%i
FOR %%i in (*.inp) do (
echo Processing %%i
call vscript.bat %~dp0\..\scripts\dsm2_tidy.py %%i %%i_tmp.inp
move /y %%i_tmp.inp %%i
)