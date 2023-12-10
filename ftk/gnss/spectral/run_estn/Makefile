include ../../../compiler.config

all: run_estn





SLIB = ../../data/read/gnss_data_read.a ../../data/write/gnss_data_write.a ../../data/offset/gnss_data_offset.a ../../../lib/ftk_lib.a ../../../external/gamit_globk/gg_lib.a 
X11LIB = 
II = 

run_estn : run_estn.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) run_estn.f  $(SLIB) -o run_estn $(FFLAGS)
	rm -f run_estn.o


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) run_estn $(bindir)

uninstall:
	rm -f $(bindir)/run_estn

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core run_estn  run_estn
# End of Makefile
