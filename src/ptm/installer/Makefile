# Makefile to update the catalogs for dss files
# get all files under current directory
#FTP_DIR=/support2/home/ftp/pub/delta/IEP/DSM2/source/ptm
FTP_DIR=../
PACKAGE=
#
JAVA_FILES = PTMBuilder.java ScriptFileConfigTask.java BatchFileConfigTask.java
#
PACKAGE_LOC=./
#
JAVA_HOME=/site/java/jdk
JAVAC=${JAVA_HOME}/bin/javac
JAVA=${JAVA_HOME}/bin/java
PACKAGE_HOME=./
CLASSPATH_DEF = ${PACKAGE_HOME}:/site/java/WebstartSDK3.0/classes:/site/java/jdk/lib/classes.zip

installer: compile 
	rm -rf ptm.class ptm-bin.zip
	${JAVA} -classpath ${CLASSPATH_DEF} PTMBuilder
	zip -r ptm-bin.zip ptm.class install.bat install.sh
	mv ptm-bin.zip ${FTP_DIR}
	rm -rf *.class

compile: ${JAVA_FILES}
	${JAVAC} -classpath ${CLASSPATH_DEF} -d ${PACKAGE_HOME} ${JAVA_FILES}
	
