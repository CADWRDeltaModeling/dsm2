@echo off
start jrew -cp . ptm
if not errorlevel goto end
echo jre is not installed in your path
echo download jre from http://java.sun.com and install first
:end
