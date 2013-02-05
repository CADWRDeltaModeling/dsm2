@echo off
rem ###################################
rem Batch file for running vista client
rem ###################################
setlocal
set vista_home=%~dp0/..
rem echo %vista_home%
if exist "%vista_home%/jython/jython.jar" goto :valid


:notfound

echo ############################################################
echo   Error: VISTA files not found
echo   ___
echo   Installation instructions
echo   ___
echo   The value of the environment variable vista_home in the 
echo   file vista.bat needs to match the location where
echo   VISTA has been installed
echo ############################################################
PAUSE
goto :end

:valid
rem ###############
rem Set path to location of dll
rem ###############
set path=%vista_home%/lib;%path%

rem ###############
rem starting vista
rem ###############
::start %vista_home%/jre/bin/
"%vista_home%/jre6/bin/javaw" -mx512m  -Djava.library.path="%vista_home%/lib" -Dvista.home="%vista_home%" -classpath "%vista_home%/lib/vista.jar;%vista_home%/lib/vista-help.jar;%vista_home%/jython/jython.jar;%vista_home%/lib/pd.jar;%vista_home%/lib/misc.jar;%vista_home%/lib/jhall.jar;%vista_home%/lib/jnios.jar;%vista_home%/lib/jhdf5.jar;%vista_home%/lib/jhdfobj.jar;%vista_home%/lib/jhdf5obj.jar;%vista_home%/lib/heclib.jar;%vista_home%/lib/ojdbc6.jar"  vista.app.MainGUI %1%

:end
endlocal 
rem 
