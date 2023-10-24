echo off
echo processing
FOR %%i in (*.inp) do echo %%i
FOR %%i in (*.inp) do (
echo Processing %%i
dsm2_tidy.py %%i %%i_tmp.inp
move /y %%i_tmp.inp %%i
)