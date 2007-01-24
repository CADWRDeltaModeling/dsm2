@echo off
::  usage:  prepro hydro|qual|both config-file
:: 

if {%1%}=={} (
echo "usage:  prepro config-file"
echo Prepro is needed only when the CALSIM file changes.
goto fin
)

set CONFIGFILE=%1%

rem do not add spaces to the following command
if NOT EXIST %CONFIGFILE% GOTO noconfig
echo Prepro is needed only when the CALSIM file changes.


echo Preparing Hydro
rem vscript prepro/prep_gates.py %CONFIGFILE%
vscript prepro/prep_flow.py %CONFIGFILE%
echo Done with Hydro
echo Preparing Qual
rem vscript prepro/prep_ec.py %CONFIGFILE%
rem vscript prepro/prep_vamp.py %CONFIGFILE%
echo Done with Qual
)

goto fin

:noconfig
echo %CONFIGFILE%
echo The configuration file must be specified on the command line
echo and be a valid file

:fin