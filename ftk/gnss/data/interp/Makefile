include ../../../compiler.config

all: gnss_data_interp interp_sios





GNSS_DATA_INTERP = gnss_data_interp.a
SLIB = ./gnss_data_interp.a ../read/gnss_data_read.a ../write/gnss_data_write.a ../../../lib/ftk_lib.a ../../../external/nr/nr_lib.a ../../../external/gamit_globk/gg_lib.a 
X11LIB = 
II = 

gnss_data_interp : $(GNSS_DATA_INTERP)

interp_sios : interp_sios.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) interp_sios.f  $(SLIB) -o interp_sios $(FFLAGS)
	rm -f interp_sios.o

$(GNSS_DATA_INTERP) : \
	$(GNSS_DATA_INTERP)(interp_tx.o)
	ranlib gnss_data_interp.a


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) gnss_data_interp $(bindir)
	$(INSTALL) interp_sios $(bindir)

uninstall:
	rm -f $(bindir)/gnss_data_interp
	rm -f $(bindir)/interp_sios

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core NONE interp_sios  NONE interp_sios
# End of Makefile
