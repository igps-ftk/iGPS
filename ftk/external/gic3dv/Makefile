include ../../compiler.config

all: gic3dv





X11LIB = 
II = 

gic3dv : gic3dv.f   
	$(FTN) gic3dv.f  voron.f90 -o gic3dv $(FFLAGS)
#	rm -f gic3dv.o

.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o
	
.f90.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o

install:
	$(INSTALL) gic3dv $(bindir)

uninstall:
	rm -f $(bindir)/gic3dv

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core gic3dv
# End of Makefile
