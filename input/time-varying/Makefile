PROGRAM=wrt_tmp
PROGRAM_EXE=${PROGRAM}
SOURCE0=${PROGRAM:=.f}
OBJECT=${SOURCE0:.f=.o}

SOURCES=readtvd.f readdss.f init_store_outpaths.f \
 store_outpaths.f wrt_outpaths.f writedss.f update_intervals.f \
 read_mult_tide.f read_tide_head.f read_tide_flow.f get_tidefile_dates.f \
 init_qual_bin.f	

INCLUDES=dss.inc readdss.inc writedss.inc tide.inc common_tide.f \
 common_qual_bin.inc	

SOURCES_ALL=${SOURCES} ${SOURCE0} ${INCLUDES}

OBJECTS=${SOURCES:.f=.o}
LIB=lib.a
MEMBERS=${OBJECTS}

LIBFIX=../fixed/${LIB}
optmax plain install:= LIBDSS=/usr/hec/lib/heclib.a
dbg := LIBDSS=/usr/hec/lib/heclib-dbg.a

.KEEP_STATE:
.PRECIOUS: ${LIBFIX} ${LIBDSS} ${SOURCES} ${RCSFILES}

none:
	@ echo 'Usage: make src|dbg|optmax|mix|install|clean '\
	 1>&2; exit 2

dbg optmax mix install plain: ${LIB} ${PROGRAM_EXE}
src: ${SOURCES_ALL}

COMPILE_FLAG=
LOCAL_FLAG= -e -Nl99

dbg := FFLAGS= ${LOCAL_FLAG} -C -g ${COMPILE_FLAG}
plain := FFLAGS= ${LOCAL_FLAG} ${COMPILE_FLAG}
optmax install:= FFLAGS= ${LOCAL_FLAG} -dalign -fast -O4 -depend ${COMPILE_FLAG}

${PROGRAM_EXE}: ${OBJECT} ${LIB} ${LIBDSS}
	${LINK.F} -o $@ ${OBJECT} ${LIBFIX} ${LIB} ${LIBDSS}  \
          ../../hydro/lib.a

${LIB}: ${OBJECTS}
	rm -f ${LIB}
	ar cq ${LIB} ${OBJECTS}

install: ${PROGRAM_EXE}
	/usr/ucb/install -s ${PROGRAM_EXE} /site/bin/${PROGRAM}

#${SOURCES_ALL}: RCS/$$@,v
#	${CO} ${rcsflags} $@

clean:
	rm -f ${OBJECTS} ${LIB}

FORCE:
