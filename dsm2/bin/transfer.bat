@echo off
::Launches the script transfer.py
::This script allows you to copy time series from one DSS file to another
::Optionally, you can slice the time series in time
::Optionally, you can perform a transformation (period ave rio 
SET bindir=%~dp0
SET p1=%1
SET p2=%2
SET p3=%3
SHIFT
SHIFT
SHIFT
call vscript.bat %bindir%\..\scripts\transfer.py %p1% %p2% %p3% %%1 %%2 %%3 %%4 %%5 %%6 %%7 %%8 %%9


