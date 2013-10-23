@echo off

if exist %dsm2_home%/bin/ptm.jar goto :valid

:notfound
echo ## An Error has occured in the batch file. DSM2_HOME not set correctly or ptm.jar not found.##
PAUSE
goto :end

:valid

set path=%dsm2_home%/bin;%path%

"%vista_home%/jre6/bin/java" -ss1m -mx1024m -oss1m -classpath "%dsm2_home%/bin/ptm.jar;%dsm2_home%/bin/COM.jar;%dsm2_home%/bin/edu.jar;%dsm2_home%/bin/xml.jar" DWR.DMS.PTM.MainPTM %1

:end


