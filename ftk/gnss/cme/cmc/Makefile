include ../../../compiler.config

all: cmc_cal_optimal




SLIB = ../corr/gnss_cme_corr.a ../../data/site/gnss_data_site.a ../../data/read/gnss_data_read.a ../../data/write/gnss_data_write.a ../../../lib/ftk_lib.a ../../../external/gamit_globk/gg_lib.a 
X11LIB = 
II = 

cmc_cal_optimal : cmc_cal_optimal.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) cmc_cal_optimal.f  $(SLIB) -o cmc_cal_optimal $(FFLAGS)
	rm -f cmc_cal_optimal.o


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) cmc_cal_optimal $(bindir)

uninstall:
	rm -f $(bindir)/cmc_cal_optimal

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core cmc_cal_optimal  cmc_cal_optimal
# End of Makefile
