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
set SCRIPT_HOME=%DSM2_HOME%/scripts
call vscript ./scripts/extend_calsim_outputs_bst.py %CONFIGFILE%
call vscript %SCRIPT_HOME%/prep_stage.py %CONFIGFILE%
call vscript ./scripts/planning_boundary_flow_bst.py %CONFIGFILE%
call vscript %SCRIPT_HOME%/prep_dicu_flow.py %CONFIGFILE%
call vscript %SCRIPT_HOME%/prep_gates.py %CONFIGFILE%
rem call vscript %SCRIPT_HOME%/prep_vamp.py %CONFIGFILE%
call vscript ./scripts/prep_ec_bst.py %CONFIGFILE%
call vscript ./scripts/prep_doc_bst.py %CONFIGFILE%
call vscript ./scripts/dailymapping_051010.py %CONFIGFILE%
)

goto fin

:noconfig
echo %CONFIGFILE%
echo The configuration file must be specified on the command line
echo and be a valid file

:fin