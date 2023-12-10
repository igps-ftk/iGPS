include ../../../compiler.config

all: ts_add ts_minus





SLIB = ../read/gnss_data_read.a ../write/gnss_data_write.a ../../../lib/ftk_lib.a ../../../external/gamit_globk/gg_lib.a 
X11LIB = 
II = 

ts_add : ts_add.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) ts_add.f  $(SLIB) -o ts_add $(FFLAGS)
	rm -f ts_add.o

ts_minus : ts_minus.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) ts_minus.f  $(SLIB) -o ts_minus $(FFLAGS)
	rm -f ts_minus.o


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) ts_add $(bindir)
	$(INSTALL) ts_minus $(bindir)

uninstall:
	rm -f $(bindir)/ts_add
	rm -f $(bindir)/ts_minus

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core ts_add ts_minus  ts_add ts_minus
# End of Makefile
