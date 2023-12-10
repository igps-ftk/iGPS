include ../../../compiler.config

all: run_cats





SLIB = ../../data/read/gnss_data_read.a ../../data/write/gnss_data_write.a ../../../lib/ftk_lib.a ../../../external/gamit_globk/gg_lib.a 
X11LIB = 
II = 

run_cats : run_cats.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) run_cats.f  $(SLIB) -o run_cats $(FFLAGS)
	rm -f run_cats.o


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) run_cats $(bindir)

uninstall:
	rm -f $(bindir)/run_cats

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core run_cats  run_cats
# End of Makefile
