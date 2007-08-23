@echo off
:: Batch file directions: 
:: In order to run a planning study, you need to specify the model and
::    a configuration file at the command line, e.g.: 
::     D:\MYSTUDYDIR> dsm2 hydro configuration.inp

SETLOCAL
set DSM2MODULE=%1%
set CONFIGFILE=%2%
set MODELINPUT=%3%

if {%DSM2MODULE%} == {} goto helpmsg

rem do not add spaces to the following command
if {%CONFIGFILE%} == {} goto noconfig
if not exist %CONFIGFILE% goto noconfig

set HYDRO_COMMAND=^echo Not executing hydro
set QUAL_COMMAND=^echo Not executing qual

if %DSM2MODULE%==hydro (
type %CONFIGFILE% > hydrotemp.inp & type hydro.inp >> hydrotemp.inp
set HYDRO_COMMAND=hydro hydrotemp.inp
) 
if %DSM2MODULE%==qual_ec (
type %CONFIGFILE% > qualectemp.inp & type qual_ec.inp >> qualectemp.inp
set QUAL_COMMAND=qual qualectemp.inp
)
if %DSM2MODULE%==qual_do (
type %CONFIGFILE% > qualdotemp.inp & type qual_do.inp >> qualdotemp.inp
set QUAL_COMMAND=qual qualdotemp.inp
)
if %DSM2MODULE%==both (
type %CONFIGFILE% > hydrotemp.inp & type hydro.inp >> hydrotemp.inp
type %CONFIGFILE% > qualectemp.inp & type qual_ec.inp >> qualectemp.inp
set HYDRO_COMMAND=hydro hydrotemp.inp
set QUAL_COMMAND=qual qualectemp.inp
)

%HYDRO_COMMAND% & if exist hydrotemp.inp del hydrotemp.inp
%QUAL_COMMAND% & if exist qual*temp.inp del qual*temp.inp
goto end

:helpmsg
echo "Usage: dsm2.bat hydro|qual_ec|qual_do|both config.inp"
goto end

:noconfig
if NOT {%CONFIGFILE%} == {} echo %CONFIGFILE%
echo The configuration file must be specified on the command line
echo and be a valid file
goto end


:end
ENDLOCAL