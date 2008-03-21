@echo off
::  usage:  prepro hydro|qual|both config-file
:: 

if {%1%}=={} (
echo "usage:  prepro qual config-file"
goto fin
)

if {%2%}=={} (
echo "usage:  prepro qual config-file"
goto fin
)

set DSM2MODULE=%1%
set CONFIGFILE=%2%

if NOT EXIST %CONFIGFILE% GOTO noconfig

if %DSM2MODULE%==qual (
echo Preparing Qual
vscript prepro/prepEC.py %CONFIGFILE%
echo Done with Qual
)

if %DSM2MODULE%==both (
echo Preparing Qual
vscript prepro/prepEC.py %CONFIGFILE%
echo Done with Qual
)

goto fin

:noconfig
echo %CONFIGFILE%
echo The configuration file must be specified on the command line
echo and be a valid file

:fin