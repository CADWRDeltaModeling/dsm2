# Makefile for new model with debug, optimize options.

VN:sh=../version.sh version.inc

PROGRAM=qual
PROGRAM_EXE=${PROGRAM}
SOURCE0=${PROGRAM:=.f}
OBJECT=${SOURCE0:.f=.o}

SOURCES1=\
balanceflo.f bltminit.f calscsk.f checkerror.f get_output.f heat.f \
interpx.f kinetic.f process_tide.f rate_chanres.f route.f \
masstrack.f 

SOURCES=${SOURCE0} ${SOURCES1}
OBJECTS1=${SOURCES1:.f=.o}

LIB=lib.a
LIBFIX=../input/fixed/${LIB}
LIBTV=../input/time-varying/${LIB}
optmax mix := LIBLOC=/usr/hec/code/local/lib.a
dbg := LIBLOC=/usr/hec/code/local/lib-dbg.a
optmax mix := LIBDSS=/usr/hec/lib/heclib.a
dbg := LIBDSS=/usr/hec/lib/heclib-dbg.a

MEMBERS=${SOURCES:.f=.o}

.KEEP_STATE:
.PRECIOUS: ${LIBFIX} ${LIBTV} ${LIBLOC} ${LIBDSS} ${SOURCES} ${RCSFILES}

none:
	@ echo 'Usage: make src|dbg|opt|optmax|mix|install|clean '\
	 1>&2; exit 2

dbg optmax mix : ${PROGRAM_EXE}
src: ${SOURCES} ${LIBFIX} ${LIBTV}

COMPILE_FLAG=
LOCAL_FLAG= -e -Nl99 -dalign
#LDFLAGS=-xildoff

dbg := FFLAGS= ${LOCAL_FLAG} -C -g -fnonstd ${COMPILE_FLAG}
optmax := FFLAGS= ${LOCAL_FLAG} -fast -O3 -depend ${COMPILE_FLAG}
mix := FFLAGS= ${LOCAL_FLAG} -fast -O3 -depend ${COMPILE_FLAG}

src := TARGET = src
dbg := TARGET = dbg
mix := TARGET = dbg
optmax := TARGET = optmax
clean := TARGET = clean

${PROGRAM_EXE}: ${OBJECT} ${LIB} ${LIBTV} ${LIBFIX} \
   ${LIBLOC} ${LIBDSS}
	${LINK.F} -o $@ ${OBJECT} ${LIB} ${LIBFIX} ${LIBTV} \
        ${LIBLOC} ${LIBDSS} ${LIB}

${LIBFIX}: FORCE
	cd ${@D}; make ${TARGET}

${LIBTV}: FORCE
	cd ${@D}; make ${TARGET}

${LIB}: ${OBJECTS1}
	rm -f ${LIB}
	ar cq ${LIB} ${OBJECTS1}

install: optmax
	/usr/ucb/install -s ${PROGRAM_EXE} /site/bin/${PROGRAM}
	/usr/ucb/install -s ${PROGRAM_EXE} /site/bin/${PROGRAM_EXE}-${VN}
#${SOURCES}: RCS/$$@,v
#	${CO} ${rcsflags} $@

clean: ${LIBFIX} ${LIBTV}
	rm -f ${LIB} ${OBJECT} ${OBJECTS1} ${PROGRAM_EXE}

FORCE:
