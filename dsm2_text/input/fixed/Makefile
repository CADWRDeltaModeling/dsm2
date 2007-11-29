.KEEP_STATE:

FP:sh=fpversion -foption

CC=acc


SOURCE1=${PROGRAM1:=.f}
SOURCE2=${PROGRAM2:=.f}
OBJECT1=${SOURCE1:.f=.o}
OBJECT2=${SOURCE2:.f=.o}

SOURCES=dsm2_init.f read_fixed.f process_fixed.f \
check_fixed.f check_fixed_hydro.f \
print_output.f readirreg.f utilities.f \
check_fixed_qual.f print_outqual.f \
check_fixed_ptm.f init_ptm.f

SOURCES_C=qsort.c

INCLUDES=common.f misc.f common_irreg_geom.f

SOURCES_ALL=${SOURCES} ${SOURCE1} ${SOURCE2} ${INCLUDES}

OBJECTS=${SOURCES:.f=.o} ${SOURCES_C:.c=.o}
LIB=lib.a
MEMBERS=${OBJECTS}

optmax plain := LIBDSS=/usr/hec/lib/heclib.a
dbg := LIBDSS=/usr/hec/lib/heclib-dbg.a

.PRECIOUS: ${LIBDSS} ${SOURCES_ALL} ${RCSFILES}

none:
	@ echo 'Usage: make src|dbg|optmax|mix|install|clean '\
	 1>&2; exit 2

dbg optmax mix install plain: ${LIB}
src: ${SOURCES_ALL}

LOCAL_FLAG= -e -Nl99

dbg := FFLAGS= ${LOCAL_FLAG} -C -g
dbg := CFLAGS= -g -fsingle
plain := FFLAGS= ${LOCAL_FLAG}
plain := CFLAGS= -fsingle
optmax install := FFLAGS= ${LOCAL_FLAG} -dalign -O1
optmax install := CFLAGS= -dalign -fsingle -O4


${LIB}: ${OBJECTS}
	rm -f ${LIB}
	ar cq ${LIB} ${OBJECTS}

install:
	
#${SOURCES_ALL}: RCS/$$@,v
#	${CO} ${rcsflags} $@

clean:
	rm -f ${OBJECTS} ${LIB}

FORCE:
