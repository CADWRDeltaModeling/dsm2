set JAVA_COMPILER=NONE
set JDK_HOME=c:\progra~1\jdk1.31_05

cd dialog
%JDK_HOME%\bin\javac -g -deprecation -classpath c:\progra~1\jdk1.31_05\lib\classes.zip;c:\progra~1\vista\lib\vista.jar;c:\progra~1\vista\lib\pd.jar;d:\java\csdpDistribution\swing\csdp\lib\jpy.jar *.java
copy *.class d:\java\classes\DWR\CSDP\dialog

cd ..
cd semmscon
%JDK_HOME%\bin\javac *.java
copy *.class d:\java\classes\DWR\CSDP\semmscon

cd ..
%JDK_HOME%\bin\javah -classpath d:\java\classes -jni DWR.CSDP.semmscon.UseSemmscon
copy DWR_CSDP_semmscon_UseSemmscon.h semmscon
cd semmscon
bcc32 -WD -RT- -x- -v -Id:\java\csdp\semmscon DWR_CSDP_semmscon_utmconvert semmscon.lib

cd ..
%JDK_HOME%\bin\javac -g -deprecation -classpath c:\progra~1\jdk1.31_05\lib\classes.zip;c:\progra~1\vista\lib\vista.jar;c:\progra~1\vista\lib\pd.jar;d:\java\csdpDistribution\swing\csdp\lib\jpy.jar;d:\java\classes\ *.java

copy *.class d:\java\classes\DWR\CSDP
cd images
copy *.gif d:\java\classes\DWR\CSDP\images
copy *.jpg d:\java\classes\DWR\CSDP\images

cd d:\java\classes
%JDK_HOME%\bin\jar -cvf csdp.jar COM DWR/CSDP
cd d:\java\csdp

