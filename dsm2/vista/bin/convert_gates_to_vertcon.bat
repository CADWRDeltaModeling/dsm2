@echo off
::  usage: convert_xsects_to_vertcon.bat hydro_echo.inp gis.inp
echo 
echo -------------------------------------
echo     Calculates lat/long for xsections and writes out to a vertcon.in file that 
echo	   can be used with the vertcon.exe program to calculate the corrections to be
echo	   applied
echo     The echofile and the gis data is needed to place the xsections.
echo     Search dsm2 grid map on website for more information on exact format
echo	 Verify the lat/longs in the vertcon.in file produced by this program
echo     Run vertcon as follows :-
echo     vertcon < vertcon.ctrl
echo     vertcon.ctrl file is created along with the vertcon.in by this program
echo     After that step use convert_xsects_from_vertcon.bat with input of vertcon.out
echo     produced by the vertcon program to get the xsections converted to NAVD88
echo -------------------------------------
echo
set VISTA_HOME=%~dp0
set SCRIPT_HOME=%~dp0/../scripts/xsection_datum_converter
%vista_home%/vscript.bat %script_home%/gate_to_vertcon.py %1 %2