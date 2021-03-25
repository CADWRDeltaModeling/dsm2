@echo on
rem ###################################
rem Batch file for running PTM
rem ###################################
setlocal
set ptm_home=%~dp0
if exist "%ptm_home%\PTM.jar" goto :valid

:notfound

echo ############################################################
echo   Error: ptm files not found
echo   ___
echo   Installation instructions
echo   ___
echo   The value of the environment variable ptm_home in the 
echo   file ptm.bat needs to match the location where
echo   ptm has been installed
echo ############################################################
PAUSE
goto :end

:valid
rem ###############
rem Set path to location of dll
rem ###############
set PATH=%ptm_home%;%ptm_home%lib;%PATH%

rem ###############
rem starting ptm
rem ###############
::start %ptm_home%/jre/bin/
"%ptm_home%jre\bin\java" -ss1m -mx64m -cp "%ptm_home%lib\edu.jar;%ptm_home%lib\COM.jar;%ptm_home%lib\xml.jar;%ptm_home%lib\commons-math3-3.6.1.jar;%ptm_home%PTM.jar"  DWR.DMS.PTM.MainPTM  %*

:end
endlocal 
rem