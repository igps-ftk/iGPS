include ../../../compiler.config

all: newm newp news newf newhtml





SLIB = ../../../lib/ftk_lib.a ../../../external/gamit_globk/gg_lib.a 
X11LIB = 
II = 

newm : newm.f  $(SLIB) 
	$(FTN) newm.f  $(SLIB) -o newm $(FFLAGS)
	rm -f newm.o

newp : newp.f  $(SLIB) 
	$(FTN) newp.f  $(SLIB) -o newp $(FFLAGS)
	rm -f newp.o

news : news.f  $(SLIB) 
	$(FTN) news.f  $(SLIB) -o news $(FFLAGS)
	rm -f news.o

newf : newf.f  $(SLIB) 
	$(FTN) newf.f  $(SLIB) -o newf $(FFLAGS)
	rm -f newf.o

newhtml : newhtml.f  $(SLIB) 
	$(FTN) newhtml.f  $(SLIB) -o newhtml $(FFLAGS)
	rm -f newhtml.o


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) newm $(bindir)
	$(INSTALL) newp $(bindir)
	$(INSTALL) news $(bindir)
	$(INSTALL) newf $(bindir)
	$(INSTALL) newhtml $(bindir)

uninstall:
	rm -f $(bindir)/newm
	rm -f $(bindir)/newp
	rm -f $(bindir)/news
	rm -f $(bindir)/newf
	rm -f $(bindir)/newhtml

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core newm newp news newf newhtml 
# End of Makefile
