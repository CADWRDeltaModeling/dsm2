@echo off
::  usage: compare output dss files
echo 
echo -------------------------------------
echo     Generate Comparison Report 
echo     for DSM2 Output DSS Files
echo -------------------------------------
echo
set VISTA_HOME=%~dp0
set SCRIPT_HOME=%~dp0/../scripts/compare_dss
%vista_home%/vscript.bat %script_home%/dsm2_dss_compare.py %1