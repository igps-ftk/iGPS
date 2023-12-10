include ../../../compiler.config

all: cme_cal_stacking




SLIB = ../../data/site/gnss_data_site.a ../../data/read/gnss_data_read.a ../../data/write/gnss_data_write.a ../../../lib/ftk_lib.a ../../../external/gamit_globk/gg_lib.a 
X11LIB = 
II = 

cme_cal_stacking : cme_cal_stacking.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) cme_cal_stacking.f  $(SLIB) -o cme_cal_stacking $(FFLAGS)
	rm -f cme_cal_stacking.o


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) cme_cal_stacking $(bindir)

uninstall:
	rm -f $(bindir)/cme_cal_stacking

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core cme_cal_stacking  cme_cal_stacking
# End of Makefile
