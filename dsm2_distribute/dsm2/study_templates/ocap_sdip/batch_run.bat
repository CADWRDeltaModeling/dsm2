@echo off

if "CONFIGFILE"=="" do (
echo .
echo usage:  batch_prepro configfile scenariofile [resume]
echo .
echo         where  configfile   is a study specific configuration file
echo                             such as config_sdip.inp that is batch-ready.
echo                scenariofile is a list of DSM2MODIFIERS, directories and file names (comma-separated, one per line)
echo                resume       is an optional tag to indicate that batch preprocessing
echo                               should be run only over inputs that have not already
echo                               been prepared (you may have to prune one half-finished file, usually the last one)
)

set CONFIGFILE=%1%
set SCENARIOFILE=%2%
set RESUME=fresh
if {%3%} == {resume} set RESUME=resume

:: This section iterates through the alternatives and calls 
::  the "item" procedure for each
FOR /f "tokens=1,2,3 delims=," %%u IN (%SCENARIOFILE%) DO call :item %%u %%v %%w %CONFIGFILE% %RESUME%
goto end

:helpmsg
echo "Usage: batch_prepro config.inp [resume]"
goto end

:noconfig
if NOT {%CONFIGFILE%} == {} echo %CONFIGFILE%
echo The configuration file must be specified on the command line
echo and be a valid file
goto end

:: This section preprocesses one item
:item
set alt=%1
set batch_calsimdir=%2
set batch_calsimname=%3
set config=%4
set continue_old=%5
set batch_dsm2modifier=%alt%


echo DSM2MODIFIER: %BATCH_DSM2MODIFIER%
echo CALSIMDIR: %BATCH_CALSIMDIR%
echo CALSIMNAME: %BATCH_CALSIMNAME%
echo resume: %continue_old%

hydro hydro.inp
qual qual_ec.inp

goto end

:end

:DOSEXIT
