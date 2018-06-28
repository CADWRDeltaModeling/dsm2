@echo off
:: Batch file for running ptm client

if "%ptm_home%"=="" goto noptm

:: Set path to location of dll

set path=%ptm_home%\lib;%path%

:: Start client

echo starting ptm
echo.
echo Trying to find libraries at: %ptm_home%
echo.
::echo Using 
::java -version
::java -ss1m -mx32m -cp "%ptm_home%\lib\ptm.jar;%ptm_home%\lib\edu.jar;%ptm_home%\lib\COM.jar" DWR.DMS.PTM.mainPTM
jre -ss1m -mx32m -cp "%ptm_home%\lib\ptm.jar;%ptm_home%\lib\edu.jar;%ptm_home%\lib\COM.jar" DWR.DMS.PTM.mainPTM
goto end

:noptm

echo.
echo Please set ptm_home to directory where ptm is installed
echo Example: set ptm_home=c:\ptm
echo.	

:end
