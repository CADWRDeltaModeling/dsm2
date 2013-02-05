@echo off
::  usage: compare input echo file
echo 
echo -------------------------------------
echo     Generate Comparison Report 
echo     for DSM2 Input Echo Files
echo -------------------------------------
echo
set VISTA_HOME=%~dp0
set SCRIPT_HOME=%~dp0/../scripts/compare_inp
%vista_home%/vscript.bat %script_home%/inp_compare.py
