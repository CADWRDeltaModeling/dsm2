:: This script copies the distributed historical run, then calls a script that will
:: strip out all the Informix references and replace them with Access
echo off
SETLOCAL
SET origloc=%DSM2_HOME%\studies\historic
echo Copying from %origloc%
if not exist %origloc% echo Original location %origloc% not found
xcopy /Y/Q %origloc%\*.* .
move hydro.inp hydro.inp.move_to_access
move qual_ec.inp qual_ec.inp.move_to_access
move qual_do.inp qual_do.inp.move_to_access
move ptm.inp ptm.inp.move_to_access
move config-hist.inp config-hist.inp.move_to_access

call vscript move_to_access.py hydro.inp
call vscript move_to_access.py qual_ec.inp
call vscript move_to_access.py qual_do.inp
call vscript move_to_access.py ptm.inp
call vscript move_to_access.py config-hist.inp

ENDLOCAL
del *.*.move_to_access
if not exist .\output mkdir .\output


