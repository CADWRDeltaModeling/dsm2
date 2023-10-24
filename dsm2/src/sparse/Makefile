.KEEP_STATE:

FP:sh=fpversion -foption

CC=acc

SOURCES=spallocate.c spbuild.c spfactor.c \
spfortran.c spoutput.c spsolve.c sptest.c sputils.c

OBJECTS=${SOURCES:.c=.o}
LIB=lib.a
MEMBERS=${OBJECTS}

CO=co
rcsflags=-urfinch
RCSFILES=${SOURCES_ALL:%=RCS/%,v}

.PRECIOUS: ${SOURCES} ${RCSFILES}

none:
	@ echo 'Usage: make src|dbg|optmax|mix|install|clean '\
	 1>&2; exit 2

dbg optmax mix install plain: ${LIB}
src: ${SOURCES}

COMPILE_FLAG=-fsingle -pic

dbg := CFLAGS= -g ${COMPILE_FLAG}
plain := CFLAGS= -fast -O4 -depend ${COMPILE_FLAG}
optmax install := CFLAGS= -fast -dalign -O5 -depend

${LIB}: ${OBJECTS}
	rm -f ${LIB}
	ar cq ${LIB} ${OBJECTS}

#${SOURCES}: RCS/$$@,v
#	${CO} ${rcsflags} $@

clean:
	rm -f ${OBJECTS} ${LIB}

FORCE:
