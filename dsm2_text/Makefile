# Makefile for DSM2; all modules.

none:
	@ echo 'Usage: make src|dbg|optmax|install|clean '\
	 1>&2; exit 2

src := TARGET = src
dbg := TARGET = dbg
optmax := TARGET = optmax
install := TARGET = install
clean := TARGET = clean

src dbg optmax install clean: FORCE
	cd hydro; make ${TARGET}
	cd qual; make ${TARGET}
	cd ptm; make ${TARGET}

FORCE:
