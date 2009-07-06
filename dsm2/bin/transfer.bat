@echo off
::Launches the script transfer.py
::This script allows you to copy time series from one DSS file to another
::Optionally, you can slice the time series in time
::Optionally, you can perform a transformation (period ave rio 

call vscript.bat %%~dp0\..\scripts\transfer.py %%1 %%2 %%3 %%4 %%5 %%6 %%7 %%8



