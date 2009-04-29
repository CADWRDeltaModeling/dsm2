@echo off
rem ###############################
rem Batch file for running vplotter
rem ###############################

if exist %vista_home%/jython/jython.jar goto :valid

:notfound

echo ############################################################
echo   Installation instructions
echo   ___
echo   The value of the environment variable vista_home in the 
echo   file vplotter.bat needs to match the location where
echo   VISTA has been installed
echo ############################################################
PAUSE
goto :end

:valid
rem ###############
rem Set path to location of dll
rem ###############
path=%path%;%vista_home%/bin;%vista_home%/lib;
rem ###############
rem starting vplotter
rem ###############
java -mx256m  -Djava.library.path="%vista_home%/lib" -Dvista.home="%vista_home%" -Dpython.home="%vista_home%/jython" -Dpython.path="%vista_home%/jython/Lib;%vista_home%/lib/Lib" -classpath "%vista_home%/lib/vista.jar;%vista_home%/lib/vista-help.jar;%vista_home%/jython/jython.jar;%vista_home%/lib/pd.jar;%vista_home%/lib/misc.jar;%vista_home%/lib/jhall.jar;%vista_home%/lib/jnios.jar;" org.python.util.jython %vista_home%/bin/vplotter.py -f "%1%"

:end



