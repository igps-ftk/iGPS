include ../../../compiler.config

all: outliers





SLIB = ../read/gnss_data_read.a ../write/gnss_data_write.a ../../../lib/ftk_lib.a ../../../external/gamit_globk/gg_lib.a 
X11LIB = 
II = 

outliers : outliers.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) outliers.f  $(SLIB) -o outliers $(FFLAGS)
	rm -f outliers.o


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) outliers $(bindir)

uninstall:
	rm -f $(bindir)/outliers

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core outliers  outliers
# End of Makefile
