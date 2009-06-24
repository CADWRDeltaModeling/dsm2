@echo off


if exist %dsm2_home%/bin/ptm.jar goto :valid

:notfound
echo ## An Error has occured in the batch file. PTM_HOME not set correctly or ptm.jar not found.##
PAUSE
goto :end

:valid

set path=%dsm2_hom3%/bin;%dsm2_home%/lib;%path%

java -ss1m -mx128m -oss1m -classpath "%dsm2_home%/bin/ptm.jar;%dsm2_home%/bin/COM.jar;%dsm2_home%/bin/edu.jar;%dsm2_home%/bin/xml.jar" DWR.DMS.PTM.MainPTM %1

:end


