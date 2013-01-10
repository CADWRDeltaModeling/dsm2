@echo off

if "%dsm2_home%" == "" goto :DSM2_HOME_error
if exist "%dsm2_home%"/bin/ptm.jar goto :check_arg

:notfound
echo ## An Error has occured in the batch file. DSM2_HOME not set correctly or ptm.jar not found.##
PAUSE
goto :end

:check_arg
if not "%1" == "" goto :valid
echo Error: The input file is missing.
echo Usage: %0 input_file.inp
goto :end

:DSM2_HOME_error
echo DSM2_HOME is not defined.  Check the installation of DSM2.
goto :end

:valid
java -ss1m -mx512m -oss1m -classpath "%dsm2_home%/bin/ptm.jar;%dsm2_home%/bin/COM.jar;%dsm2_home%/bin/edu.jar;%dsm2_home%/bin/xml.jar" DWR.DMS.PTM.MainPTM %1
goto :end

:end


