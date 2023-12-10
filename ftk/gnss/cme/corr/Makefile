include ../../../compiler.config

all: ts_corr simul_corr_length test_corr




GNSS_CME_CORR = gnss_cme_corr.a

SLIB = ../../data/site/gnss_data_site.a ../../data/read/gnss_data_read.a ../../data/write/gnss_data_write.a ../../../lib/ftk_lib.a ../../../external/gamit_globk/gg_lib.a ./gnss_cme_corr.a 
X11LIB = 
II = 

ts_corr : ts_corr.f $(GNSS_CME_CORR) $(SLIB) ../../../inc/ftk.h
	$(FTN) ts_corr.f $(GNSS_CME_CORR) $(SLIB) -o ts_corr $(FFLAGS)
	rm -f ts_corr.o

ts_corr_simu : ts_corr_simu.f $(GNSS_CME_CORR) $(SLIB) ../../../inc/ftk.h
	$(FTN) ts_corr_simu.f $(GNSS_CME_CORR) $(SLIB) -o ts_corr_simu $(FFLAGS)
	rm -f ts_corr_simu.o

simul_corr_length : simul_corr_length.f $(GNSS_CME_CORR) $(SLIB) ../../../inc/ftk.h
	$(FTN) simul_corr_length.f $(GNSS_CME_CORR) $(SLIB) -o simul_corr_length $(FFLAGS)
	rm -f simul_corr_length.o

test_corr : test_corr.f $(GNSS_CME_CORR) $(SLIB) ../../../inc/ftk.h
	$(FTN) test_corr.f $(GNSS_CME_CORR) $(SLIB) -o test_corr $(FFLAGS)
	rm -f test_corr.o

$(GNSS_CME_CORR) : \
	$(GNSS_CME_CORR)(corr_xy.o) \
	$(GNSS_CME_CORR)(corr_xy2.o) \
	$(GNSS_CME_CORR)(corr_xy3.o) \
	$(GNSS_CME_CORR)(lin_corr_xy.o) \
	$(GNSS_CME_CORR)(lin_corr_xy2.o) \
	$(GNSS_CME_CORR)(read_corr_snx.o)
	ranlib gnss_cme_corr.a

$(GNSS_CME_CORR)(read_corr_snx.o): ../../../inc/ftk.h

.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) ts_corr $(bindir)
	$(INSTALL) simul_corr_length $(bindir)
	$(INSTALL) test_corr $(bindir)

uninstall:
	rm -f $(bindir)/ts_corr
	rm -f $(bindir)/simul_corr_length
	rm -f $(bindir)/test_corr

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core ts_corr ts_corr_simu simul_corr_length test_corr  ts_corr ts_corr_simu simul_corr_length test_corr
# End of Makefile
