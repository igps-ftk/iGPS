include ../../../compiler.config

all: hello





SLIB = 
X11LIB = 
II = 

hello : hello.f  $(SLIB) 
	$(FTN) hello.f  $(SLIB) -o hello $(FFLAGS)
	rm -f hello.o


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o

install:
	$(INSTALL) hello $(bindir)

uninstall:
	rm -f $(bindir)/hello

install:
	$(INSTALL) hello $(bindir)

uninstall:
	rm -f $(bindir)/hello

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core hello  hello
# End of Makefile
