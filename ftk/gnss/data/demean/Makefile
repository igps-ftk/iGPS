include ../../../compiler.config

all: ts_demean





SLIB = ../read/gnss_data_read.a ../write/gnss_data_write.a ../../../lib/ftk_lib.a ../../../external/gamit_globk/gg_lib.a 
X11LIB = 
II = 

ts_demean : ts_demean.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) ts_demean.f  $(SLIB) -o ts_demean $(FFLAGS)
	rm -f ts_demean.o


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) ts_demean $(bindir)

uninstall:
	rm -f $(bindir)/ts_demean

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core ts_demean  ts_demean
# End of Makefile
