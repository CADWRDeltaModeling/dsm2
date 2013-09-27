@echo off
::  usage:  postpro_ec_fp.bat config-file
::  Update param and 

if {%1%}=={} (
echo "usage:  postpro_ec_fp.bat config-file
goto fin
)

set CONFIGFILE=%1%
if NOT EXIST %CONFIGFILE% GOTO noconfig

rem #Update total number of DSS records#
set /a ncnt=819
rem # Update param (EC_FP, VOL_FP, 200nodes_FP)#
set param=VOL_FP

set /a flag = 0
set /a i=0
:itr
set /a i=%i%+1
set /a sind = (%i%-1)*10 + 1
set /a eind = %sind% + 9
if %flag% equ 1 (goto :last)
call vscript ./scripts/postpro_fp.py %CONFIGFILE% %param% %sind% %eind% 
set /a check = %eind%+9
if %check% lss %ncnt% (goto :itr)
set /a flag = 1
goto :itr
:last
call vscript ./scripts/postpro_fp.py %CONFIGFILE% %param% %sind% %ncnt% 
goto fin

:noconfig
echo %CONFIGFILE%
echo The configuration file must be specified on the command line
echo and be a valid file

:fin