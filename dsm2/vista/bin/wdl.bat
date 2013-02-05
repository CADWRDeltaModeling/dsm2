@echo off
::  usage: wdl filename.dss
echo 
echo -------------------------------------
echo     Downloads water data library data for water quality stations
echo -------------------------------------
echo
set scripts_home=%~dp0/../scripts
%~dp0/vscript.bat %scripts_home%/wdl/wdl.py %1

