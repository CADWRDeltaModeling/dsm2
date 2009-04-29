@echo off
rem ###############
rem Batch file for running vserver
rem ###############

set vista_home=%~dp0%../

if exist "%vista_home%/jython/jython.jar" goto :valid

:notfound

echo ############################################################
echo   Installation instructions
echo   ___
echo   The value of the environment variable vista_home in the 
echo   file vserver.bat needs to match the location where
echo   VISTA has been installed
echo ############################################################
PAUSE
goto :end

:valid
rem ###############
rem Start registry with class path
rem ###############
rem ###############
rem Set path to location of dll
rem ###############
path="%path%;%vista_home%/bin;%vista_home%/lib;"
rem ###############
rem starting vserver
rem ###############
start "VSERVER" /min "%vista_home%/jre/bin/java" -mx128m  -Djava.library.path="%vista_home%/lib" -Dvista.home="%vista_home%" -classpath "%vista_home%/lib/vista.jar;%vista_home%/lib/vista-help.jar;%vista_home%/jython/jython.jar;%vista_home%/lib/pd.jar;%vista_home%/lib/misc.jar;%vista_home%/lib/swingall.jar;%vista_home%/lib/jhall.jar;%vista_home%/lib/jnios.jar;" vista.db.dss.DSSRemoteServer

:end
