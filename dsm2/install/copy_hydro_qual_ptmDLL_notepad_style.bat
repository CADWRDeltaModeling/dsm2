set version=v8.0.6

echo version "%version%" 
set dsm2_build_dir=..\..\..\..\models\dsm2_v8_0
set dsm2_distribute_dir=..\..\..\..\models\dsm2_distribute

copy %dsm2_build_dir%\build_vs2008sp1_if11\all\Release\qual.exe   %dsm2_distribute_dir%\dsm2\bin\* 
copy %dsm2_build_dir%\build_vs2008sp1_if11\all\Release\hydro.exe  %dsm2_distribute_dir%\dsm2\bin\*
copy %dsm2_build_dir%\build_vs2008sp1_if11\all\DLL\ptm.dll        %dsm2_distribute_dir%\dsm2\bin\* 
copy %dsm2_build_dir%\src\ptm\lib\ptm.jar                         %dsm2_distribute_dir%\dsm2\bin\* 

copy %dsm2_build_dir%\build_vs2008sp1_if11\all\Release\qual.exe   %dsm2_distribute_dir%\dsm2\bin\qual_%version%.exe
copy %dsm2_build_dir%\build_vs2008sp1_if11\all\Release\hydro.exe  %dsm2_distribute_dir%\dsm2\bin\hydro_%version%.exe
copy %dsm2_build_dir%\build_vs2008sp1_if11\all\DLL\ptm.dll        %dsm2_distribute_dir%\dsm2\bin\ptm_%version%.dll 
rem this assumes a manual step that is not appropriate. Copy it from the build directory, after building it of course!!!
copy %dsm2_build_dir%\src\ptm\lib\ptm.jar                         %dsm2_distribute_dir%\dsm2\bin\ptm_%version%.jar


copy /y %dsm2_build_dir%\src\input_storage\component.py .
copy /y %dsm2_build_dir%\src\input_storage\userDefineLangTemplate.xml .

start /wait python component.py notepad

copy /y .\userDefineLang.xml %dsm2_distribute_dir%\dsm2\extras\"notepad++"\*


del .\component.py
del .\userDefineLangTemplate.xml
del .\userDefineLang.xml
 


pause