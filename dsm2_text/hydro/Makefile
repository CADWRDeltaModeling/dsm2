# Makefile for new model with debug, optimize options.

FP:sh=fpversion -foption
VN:sh=../version.sh version.inc

PROGRAM1=fourpt
#PROGRAM2=display_tidefile
PROGRAM1_EXE=hydro
#PROGRAM2_EXE=${PROGRAM2}
SOURCE1=${PROGRAM1:=.f}
SOURCE2=${PROGRAM2:=.f}
OBJECT1=${SOURCE1:.f=.o}
OBJECT2=${SOURCE2:.f=.o}

SOURCES1=\
chcnstrt.f chcnstrutils.f \
chcxtbl.f chschmt.f chstatus.f fileutil.f floweq1d.f \
get_output.f linear.f netblnce.f netbnd.f \
netcntr1.f netdense.f process_tide.f \
strings.f table3.f virtual_xsect.f

# these will be specially optimized
SOURCES2=\
solvealloc.f solvesparse.f solveutil.f

SOURCES=${SOURCE1} ${SOURCES1} ${SOURCE2}

SOURCES_ALL=${SOURCES}

OBJECTS1=${SOURCES1:.f=.o}
OBJECTS2=${SOURCES2:.f=.o}

LIB=lib.a
LIBSPARSE=sparse/${LIB}
LIBFIX=../input/fixed/${LIB}
LIBTV=../input/time-varying/${LIB}
PATCH=patch.o

mix optmax plain install := LIBDSS=/usr/hec/lib/heclib.a
dbg := LIBDSS=/usr/hec/lib/heclib-dbg.a

MEMBERS=${SOURCES:.f=.o}

.KEEP_STATE:
.PRECIOUS: ${LIBSPARSE} ${LIBFIX} ${LIBTV} ${LIBDSS} ${SOURCES} ${RCSFILES}

none:
	@ echo 'Usage: make src|dbg|plain|optmax|install|clean '\
	 1>&2; exit 2

dbg plain optmax install mix: ${PROGRAM1_EXE} ${PROGRAM2_EXE}
src: ${SOURCES_ALL} ${LIBSPARSE} ${LIBFIX} ${LIBTV}

LOCAL_FLAG=-e -Nl99

dbg := FFLAGS= ${LOCAL_FLAG} -C -g
plain := FFLAGS= ${LOCAL_FLAG}
optmax mix install := FFLAGS= ${LOCAL_FLAG} -fast -dalign -O3 -depend

src := TARGET = src
dbg := TARGET = dbg
plain := TARGET = plain
mix := TARGET = dbg
optmax := TARGET = optmax
install := TARGET = optmax
clean := TARGET = clean

${PROGRAM1_EXE}: ${OBJECT1} ${LIB} ${LIBSPARSE} ${LIBFIX} ${LIBTV} ${LIBDSS}
	${LINK.F} -o $@ ${OBJECT1} ${LIB} ${LIBSPARSE} ${LIBFIX} ${LIBTV} \
        ${LIBDSS} ${LIB}

${PROGRAM2_EXE}: ${OBJECT2}
	${LINK.F} -o $@ ${OBJECT2} ${LIBFIX} ${LIBTV} ${LIB} ${LIBDSS}

${LIBSPARSE}: FORCE
	cd ${@D}; make ${TARGET}

${LIBFIX}: FORCE
	cd ${@D}; make ${TARGET}

${LIBTV}: FORCE
	cd ${@D}; make ${TARGET}

${LIB}: ${OBJECTS1} $(SOURCES2)
	rm -f ${LIB}
	make -f Makefile-opt SOURCES2="${SOURCES2}"
	ar cq ${LIB} ${OBJECTS1}

clean: ${LIBSPARSE} ${LIBFIX} ${LIBTV}
	rm -f ${LIB} ${OBJECT1} ${OBJECTS1} ${PROGRAM1_EXE} ${PROGRAM2_EXE}

install: /site/bin/${PROGRAM1_EXE}
	/usr/ucb/install -s ${PROGRAM1_EXE} /site/bin/${PROGRAM1_EXE}-${VN}
	/usr/ucb/install -s ${PROGRAM1_EXE} /site/bin/${PROGRAM1_EXE}
FORCE:
