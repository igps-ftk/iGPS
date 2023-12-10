include ../../../compiler.config

all: llhxyz





SLIB = 
X11LIB = 
II = 

llhxyz : llhxyz.f  $(SLIB) 
	$(FTN) llhxyz.f  $(SLIB) -o llhxyz $(FFLAGS)
	rm -f llhxyz.o


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) llhxyz $(bindir)

uninstall:
	rm -f $(bindir)/llhxyz

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core llhxyz  
# End of Makefile
