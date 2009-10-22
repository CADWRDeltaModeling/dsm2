@echo off

if exist %dsm2_home%/bin/ptm.jar goto :valid

:notfound
echo ## An Error has occured in the batch file. DSM2_HOME not set correctly or ptm.jar not found.##
PAUSE
goto :end

:valid

java -ss1m -mx512m -oss1m -classpath "%dsm2_home%/bin/ptm.jar;%dsm2_home%/bin/COM.jar;%dsm2_home%/bin/edu.jar;%dsm2_home%/bin/xml.jar" DWR.DMS.PTM.MainPTM %1

:end


