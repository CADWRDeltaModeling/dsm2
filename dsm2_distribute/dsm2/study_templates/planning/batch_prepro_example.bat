@echo off
:: Batch file directions: 
:: usage:    batch_prepro config.inp [resume]
:: where  config.inp is a study specific condiguration file
::                   such as config_sdip.inp that is batch-ready.
::        resume     is an optional tag to indicate that batch preprocessing
::                   should be run only over inputs that have not already
::                   been prepared. This is a good idea if there are a lot
::                   of studies and the batch fails, but it requires that
::                   you identify and "prune" any half-prepared input files
::                   (usually the last one)
::    Description: this batch file expects a file called scenarios.txt
::    each line in this file contains an  alternative, which will become the 
::   DSM2MODIFIER
::   The :item  routine in this batch file must be changed so that the CalSim output
::  directory can be located based on the DSM2MODIFIER

set CONFIGFILE=%1%
set RESUME=fresh
if {%2%} == {resume} set RESUME=resume

:: This section iterates through the alternatives and calls 
::  the "item" procedure for each
FOR /f %%u IN (scenarios.txt) DO call :item %%u %CONFIGFILE% %RESUME%
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
set config=%2
set continue_old=%3
set batch_dsm2modifier=%alt%
set batch_calsimdir=./timeseries/CA_2020D09E_FutureNA_%alt%/D1641/DSS

echo DSM2MODIFIER: %BATCH_DSM2MODIFIER%
echo CALSIMDIR: %BATCH_CALSIMDIR%
echo resume: %continue_old%
if {%continue_old%}=={resume} if not exist .\timeseries\%BATCH_DSM2MODIFIER%.dss echo calling prepro %config% %continue_old%
if {%continue_old%}=={resume} if not exist .\timeseries\%BATCH_DSM2MODIFIER%.dss call prepro %config% %continue_old%
if {%continue_old%}=={fresh} echo call prepro %config% %continue_old%
if {%continue_old%}=={fresh} call prepro %config% %continue_old%

goto end

:end

:DOSEXIT
