include ../../../compiler.config

all: psdsios psd_stack_ray





SLIB = ../../data/read/gnss_data_read.a ../../data/write/gnss_data_write.a ../../../lib/ftk_lib.a ../../../external/gamit_globk/gg_lib.a ../../../external/nr/nr_lib.a 
X11LIB = 
II = 

psdsios : psdsios.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) psdsios.f  $(SLIB) -o psdsios $(FFLAGS)
	rm -f psdsios.o

psd_stack_ray : psd_stack_ray.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) psd_stack_ray.f  $(SLIB) -o psd_stack_ray $(FFLAGS)
	rm -f psd_stack_ray.o


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) psdsios $(bindir)
	$(INSTALL) psd_stack_ray $(bindir)

uninstall:
	rm -f $(bindir)/psdsios
	rm -f $(bindir)/psd_stack_ray

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core psdsios psd_stack_ray  psdsios psd_stack_ray
# End of Makefile
