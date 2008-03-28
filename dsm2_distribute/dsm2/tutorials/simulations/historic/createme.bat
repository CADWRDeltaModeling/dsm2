:: This script copies the distributed historical run, then calls a script that will
:: strip out all the Informix references and replace them with Access
echo off
SETLOCAL
set base=historic

SET origloc=%DSM2_HOME%\study_templates\%base%
echo Copying from %origloc%
if not exist %origloc% echo Original location %origloc% not found
xcopy /Y/Q %origloc%\*.* .

echo moving scripts to temporary location
for %%f in (hydro*.inp) do move %%f %%f.move_to_access
for %%f in (qual*.inp) do move %%f %%f.move_to_access
for %%f in (ptm*.inp) do move %%f %%f.move_to_access
for %%f in (config*.inp) do move %%f %%f.move_to_access

echo removing informix references
SET MOVE_SCRIPT=%DSM2_HOME%\vista\bin\vscript %DSM2_HOME%\scripts\move_to_access.py
for %%f in (hydro*.inp.move_to_access) do call %MOVE_SCRIPT% %%f
for %%f in (qual*.inp.move_to_access) do call %MOVE_SCRIPT% %%f
for %%f in (ptm*.inp.move_to_access) do call %MOVE_SCRIPT% %%f
for %%f in (config*.inp.move_to_access) do call %MOVE_SCRIPT% %%f

ENDLOCAL
del *.*.move_to_access
if not exist .\output mkdir .\output


:end