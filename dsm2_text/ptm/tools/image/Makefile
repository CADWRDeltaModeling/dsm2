include src.list               

CLASS_DIR=../../classes
CLASS_LOC=$(CLASS_DIR)/DWR/DMS/PTM/tools/image
CLASS_FILES=$(JAVA_FILES:%.java=${CLASS_LOC}/%.class)

FTP_DIR=/support2/home/ftp/pub/delta/IEP/DSM2/source/ptm/
BIN_DIR=../bin
LIB_DIR=../lib

.KEEP_STATE:

PRECIOUS: $(JAVA_FILES) 

JAVA = /site/java/jdk1.2.2/bin/java
JAVA_OPT = -mx32m -d ${CLASS_DIR}
#JAVA_OPT = -d ${CLASS_DIR}

JAVAC = /site/java/jdk1.2.2/bin/javac
#JAVAC = /site/java/jikes/jikes

#opt optmax:=JAVAC_OPT= -O -d $(CLASS_DIR) -deprecation -depend
opt optmax:=JAVAC_OPT= -O -d $(CLASS_DIR) -deprecation
#dbg:=JAVAC_OPT= -g -d $(CLASS_DIR) -deprecation -depend
dbg:=JAVAC_OPT= -g -d $(CLASS_DIR) -deprecation

dbg := TARGET = dbg
opt optmax := TARGET = opt
install := TARGET = install
clean := TARGET = clean

CLASSPATH_DEF= .:${CLASS_DIR}:${LIB_DIR}/xml.jar
#CLASSPATH_DEF= .:${CLASS_DIR}:/site/lib/java/classes.zip:/site/lib/java/swing.jar:/site/lib/java/jlf.jar:/site/lib/java/motif.jar:/site/lib/java/pd_classes

# document generation options
JAVADOC = /site/java/jdk1.2/bin/javadoc
JAVADOC_OPT= -classpath ../${CLASSPATH_DEF} -d docs -version -author

opt optmax dbg:
	$(JAVAC) -classpath ${CLASSPATH_DEF} $(JAVAC_OPT) ${JAVA_FILES}

docs: ${JAVA_FILES}
	${JAVADOC} ${JAVADOC_OPT} ${JAVA_FILES}

clean: 
	(cd ${CLASS_LOC};rm -rf *.class;)

writeList: $(CLASS_FILES) 
	
$(CLASS_LOC)/%.class: %.java $?
	@echo $< 

install_src:
	cp -fP `gfind . \
	  \( -name '*.java' -o -name '*.c' -o -name '*.h' -o -name '*.f' -o -name '*.inc' \) \
	  -print` $(FTP_DIR)

jar: 
	(cd ${CLASS_DIR}; jar -cf ptm.jar DWR/DMS/PTM/tools/image; mv ptm.jar ../lib)

install: opt jar

release: install
	(cd installer; make; cd ../; mv ptm-bin.zip ${FTP_DIR})

fullrelease: install release source ptm-docs

source:	install
	(cd ../ ; zip -o ptm-src.zip -r ptm -x \*.zip -x \*CVS -x \*~ -x \*,v;) 	
	mv ../ptm-src.zip ${FTP_DIR}

ptm-docs: docs
	rm -rf $(FTP_DIR)/docs
	cp -a docs $(FTP_DIR)/

FORCE:

