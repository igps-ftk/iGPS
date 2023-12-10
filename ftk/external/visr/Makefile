include ../../compiler.config

all: visr





X11LIB = 
II = 

visr : visr.f   
	$(FTN) visr.f  voron.f90 -o visr $(FFLAGS)
#	rm -f visr.o

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
	$(INSTALL) visr $(bindir)

uninstall:
	rm -f $(bindir)/visr

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core visr
# End of Makefile