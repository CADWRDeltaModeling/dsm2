@echo off
::  usage: convert_xsects_from_vertcon hydro_echo.inp gis.inp
echo 
echo -------------------------------------
echo     Converts a vertcon.out file from the vertcon program to xsections given the hydro_echo.inp and gis.inp file
echo	 This should be the step after the vertcon program has been used on the vertcon.in produced
echo	 by convert_xsects_to_vertcon
echo
echo     GIS data is needed for all nodes and channels
echo     Search dsm2 grid map on website for more information on exact format
echo -------------------------------------
echo
set VISTA_HOME=%~dp0
set SCRIPT_HOME=%~dp0/../scripts/xsection_datum_converter
%vista_home%/vscript.bat %script_home%/xsection_from_vertcon.py %1 %2