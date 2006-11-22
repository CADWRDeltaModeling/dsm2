@echo off
:: Batch file directions: 
:: In order to run a planning study, you need to specify the model and
::    a configuration file at the command line, e.g.: 
::     D:\MYSTUDYDIR> dsm2 hydro configuration.inp

set DSM2MODULE=%1%
set CONFIGFILE=%2%

if {%DSM2MODULE%} == {} goto helpmsg

rem do not add spaces to the following command
if {%CONFIGFILE%} == {} goto noconfig
if not exist %CONFIGFILE% goto noconfig

vscript prepro\dsm2.py %DSM2MODULE% %CONFIGFILE%
goto fin

:helpmsg
echo "Usage: dsm2.bat hydro|qual|both config.inp"
goto fin

:noconfig
if NOT {%CONFIGFILE%} == {} echo %CONFIGFILE%
echo The configuration file must be specified on the command line
echo and be a valid file

:fin
