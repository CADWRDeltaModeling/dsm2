#! /bin/ksh
JAVA=/site/java/jdk1.2.2/bin/java

JAVAC=/site/java/jdk1.2.2/bin/javac


JAVA_FILES=*.java

runpro="DWR.DMS.PTM.tools.image.ImageDisplayer delta.jpg plot.out"
#runpro=ReadXml
#runpro=WriteXml

${JAVA}  -classpath .:/site/lib/java/classes.zip:../../classes ${runpro}
