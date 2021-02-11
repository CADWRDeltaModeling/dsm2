SETLOCAL
REM CUSTOMIZE BELOW TO MATCH YOUR ENVIRONMENT
SET "CYGWIN_DIR=D:\cygwin"
SET "CYGWIN_PATH=%CYGWIN_DIR%\bin"
SET "PYTHON_DIR=D:\Programs\Anaconda2\envs\dsm2"
REM 32 bit java needed
SET "JAVA_HOME=C:\Program Files (x86)\Java\jdk1.6.0_45"
SET "JAVA_PATH=%JAVA_HOME%\bin"
SET "PATH=%JAVA_PATH%;%PYTHON_DIR%;%CYGWIN_PATH%;%PATH%"
SET "DSM2_THIRD_PARTY_DIR=\\cnrastore-bdo\Delta_Mod\Share\DSM2\compile_support\third_party"
CALL "c:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\bin\compilervars.bat" ia32 vs2015
REM BUILD OPRULE, INPUT STORAGE and then DSM2
ECHO Start building oprule...
cd oprule
cmake -E remove_directory BUILD
cmake -E make_directory BUILD
cd BUILD
cmake -DTHIRD_PARTY_DIR=%DSM2_THIRD_PARTY_DIR% -G "Visual Studio 14 2015" ..\ || goto :ERROR
rem cmake --build . --target ALL_BUILD --config Debug || goto :ERROR
cmake --build . --target ALL_BUILD --config Release || goto :ERROR
cd ../..

ECHO Start building input_storage...
cd input_storage
cmake -E remove_directory BUILD
cmake -E make_directory BUILD
cd BUILD
cmake -DTHIRD_PARTY_DIR=%DSM2_THIRD_PARTY_DIR% -G "Visual Studio 14 2015" ..\|| goto :ERROR
rem cmake --build . --target ALL_BUILD --config Debug || goto :ERROR
cmake --build . --target ALL_BUILD --config Release || goto :ERROR
cd ../..

ECHO Start building DSM2...
cd dsm2
cmake -E remove_directory BUILD
cmake -E make_directory BUILD
cd BUILD
cmake -DTHIRD_PARTY_DIR=%DSM2_THIRD_PARTY_DIR% -G "Visual Studio 14 2015" ..\src || goto :ERROR
rem cmake --build . --target ALL_BUILD --config Debug  || goto :ERROR
cmake --build . --target ALL_BUILD --config Release  || goto :ERROR
cpack || goto :ERROR
cd ../..

goto :END


:ERROR
echo Failed with error #%ERRORLEVEL%.
exit /b %ERRORLEVEL%

:END
echo All done. Build successful
