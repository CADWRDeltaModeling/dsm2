:: This script copies the distributed historical run, then calls a script that will
:: strip out all the Informix references and replace them with Access
echo off
SETLOCAL
set base=historical

SET origloc=%DSM2_HOME%\study_templates\%base%
echo Copying from %origloc%
if not exist %origloc% echo Original location %origloc% not found
xcopy /Y/Q %origloc%\*.* .

if not exist .\output mkdir .\output


:end