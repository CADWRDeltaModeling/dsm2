.KEEP_STATE:

LIBDIR=/site/src/dss
LIBDSSUTL=$(LIBDIR)/dssutl/lib.a
LIBDSS=$(LIBDIR)/dss/lib.a
LIBGEN=$(LIBDIR)/gen/lib.a
LIBDC=$(LIBDIR)/dc/lib.a
LIBSUN=$(LIBDIR)/sun/lib.a
LIBUNIX=$(LIBDIR)/unix/lib.a

ascii2dss: y.tab.o wrt2dss.o 
	f77 y.tab.o wrt2dss.o $(LIBDSSUTL) $(LIBDSS) $(LIBDC) $(LIBGEN) $(LIBUNIX) $(LIBSUN) -o ascii2dss

y.tab.o: y.tab.c 
	acc -DDEBUG -c -g  y.tab.c 

wrt2dss.o:  wrt2dss.f
	f77 -c -g wrt2dss.f 

y.tab.c: ascii2dss.y
	yacc ascii2dss.y

.PRECIOUS: $(LIBDSSUTL) $(LIBDSS) $(LIBDC) $(LIBGEN) $(LIBUNIX) $(LIBSUN) \
  ascii2dss.y wrt2dss.f