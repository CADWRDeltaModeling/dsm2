echo off
::dot.exe || echo "dot.exe must be on path"
set DOCFORT=doc.f90
del %DOCFORT%
::echo ! This file is for documentation purposes only. It is a workaround because doxygen cannot handle include files > doc.f90
for /R %%f IN ("..\src\transport\*.fi") DO type %%f >> %DOCFORT%
doxygen doxygen.config
