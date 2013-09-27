@echo off
::  usage:  postpro_hydro.bat config-file
::  Update param and 

if {%1%}=={} (
echo "usage:  postpro_hydro.bat config-file
goto fin
)

set CONFIGFILE=%1%
if NOT EXIST %CONFIGFILE% GOTO noconfig

rem #Update total number of DSS records#
set /a ncnt=145
rem # Update param (FLOW, STAGE, VEL, EC)#
set param=VEL

set /a flag = 0
set /a i=0
:itr
set /a i=%i%+1
set /a sind = (%i%-1)*10 + 1
set /a eind = %sind% + 9
if %flag% equ 1 (goto :last)
call vscript ./scripts/postpro.py %CONFIGFILE% %param% %sind% %eind% 
set /a check = %eind%+9
if %check% lss %ncnt% (goto :itr)
set /a flag = 1
goto :itr
:last
call vscript ./scripts/postpro.py %CONFIGFILE% %param% %sind% %ncnt% 
goto fin

:noconfig
echo %CONFIGFILE%
echo The configuration file must be specified on the command line
echo and be a valid file

:fin