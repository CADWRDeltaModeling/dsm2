@echo off
rem ###################################
rem Batch file for running vista client
rem ###################################
setlocal

if exist %vista_home%/jython/jython.jar goto :valid


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
set path=%path%;%vista_home%/lib;

rem ###############
rem starting vista
rem ###############
::start %vista_home%/jre/bin/
start java -mx256m  -Djava.library.path="%vista_home%/lib" -Dvista.home="%vista_home%" -classpath "%vista_home%/lib/vista.jar;%vista_home%/lib/vista-help.jar;%vista_home%/jython/jython.jar;%vista_home%/lib/pd.jar;%vista_home%/lib/misc.jar;%vista_home%/lib/jhall.jar;%vista_home%/lib/jnios.jar;"  vista.app.MainGUI %1%

:end
endlocal 
rem 
