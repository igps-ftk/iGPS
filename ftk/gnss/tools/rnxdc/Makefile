include ../../../compiler.config

all: rnxdc





SLIB = ../../../lib/ftk_lib.a ../../../external/gamit_globk/gg_lib.a 
X11LIB = 
II = 

rnxdc : rnxdc.f  $(SLIB) 
	$(FTN) rnxdc.f  $(SLIB) -o rnxdc $(FFLAGS)
	rm -f rnxdc.o


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) rnxdc $(bindir)

uninstall:
	rm -f $(bindir)/rnxdc

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core rnxdc  rnxdc
# End of Makefile
