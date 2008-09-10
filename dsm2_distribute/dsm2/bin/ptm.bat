@echo off


if exist %ptm_home%/lib/ptm.jar goto :valid

:notfound
echo ## An Error has occured in the batch file. PTM_HOME not set correctly or ptm.jar not found.##
PAUSE
goto :end

:valid

set path=%ptm_home%/bin;%ptm_home%/lib;%path%

java -ss1m -mx128m -oss1m -classpath "%ptm_home%/lib/ptm.jar;%ptm_home%/lib/COM.jar;%ptm_home%/lib/edu.jar;%ptm_home%/lib/xml.jar" DWR.DMS.PTM.mainPTM %1
rem java -ss1m -mx128m -classpath "%ptm_home%/classes;%ptm_home%/lib/COM.jar;%ptm_home%/lib/edu.jar;%ptm_home%/lib/xml.jar" DWR.DMS.PTM.mainPTM %1

:end


