@echo off
rem Find Java
setlocal enabledelayedexpansion
set JAVA_BIN=
if defined JAVA_HOME (
    echo JAVA_HOME is set to: %JAVA_HOME%
    for %%i in ("%JAVA_HOME%\bin\java.exe") do (
        set "JAVA_BIN=%JAVA_HOME%\bin\java.exe"
        echo Found Java: !JAVA_BIN!
    )
    if [!JAVA_BIN!] == [] (
        echo JAVA_HOME is set incorrectly.
        echo Please set JAVA_HOME to the location of your Java installation.
        goto :end
    )
) else (
    echo JAVA_HOME is not set.
    echo Please set JAVA_HOME to the location of your Java installation.
    goto :end
)

rem ###################################
rem Batch file for running PTM
rem ###################################
setlocal
set ptm_home=%~dp0..\lib
if exist "%ptm_home%\ptm.jar" goto :valid

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
set PATH=%~dp0;%PATH%

rem ###############
rem starting ptm
rem ###############
::start %ptm_home%/jre/bin/
"%JAVA_BIN%" -ss1m -mx512m -cp "%ptm_home%\edu.jar;%ptm_home%\COM.jar;%ptm_home%\xml.jar;%ptm_home%\commons-math3-3.6.1.jar;%ptm_home%\threetenbp-1.5.1.jar;%ptm_home%\ptm.jar" DWR.DMS.PTM.MainPTM %*

:end
endlocal
rem

