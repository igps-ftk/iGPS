include ../../../compiler.config

all: gnss_data_format asc2bin_sio asc2bin_sios jpl2sios trk2sios trk2hneus l_l_r2llr l_l_r2llrs l_l_r2llrs_panga pbo2sios pbo2cats pos2neu pos2qmap qocaload2cats qocaload2sios scec_csv2sio sio2cats sio2otr sio2otd sio2otx usgs_ts2sio cmonoc2sios neu_extract_by_neu qmap_extract_by_neu tenv3_to_sios





GNSS_DATA_FORMAT = gnss_data_format.a
SLIB = ../read/gnss_data_read.a ../write/gnss_data_write.a ../site/gnss_data_site.a ../../../lib/ftk_lib.a ../../../external/gamit_globk/gg_lib.a 
X11LIB = 
II = 

gnss_data_format : $(GNSS_DATA_FORMAT)

asc2bin_sio : asc2bin_sio.f  $(SLIB) 
	$(FTN) asc2bin_sio.f  $(SLIB) -o asc2bin_sio $(FFLAGS)
	rm -f asc2bin_sio.o

asc2bin_sios : asc2bin_sios.f  $(SLIB) 
	$(FTN) asc2bin_sios.f  $(SLIB) -o asc2bin_sios $(FFLAGS)
	rm -f asc2bin_sios.o

jpl2sios : jpl2sios.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) jpl2sios.f  $(SLIB) -o jpl2sios $(FFLAGS)
	rm -f jpl2sios.o

trk2sios : trk2sios.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) trk2sios.f  $(SLIB) -o trk2sios $(FFLAGS)
	rm -f trk2sios.o

trk2hneus : trk2hneus.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) trk2hneus.f  $(SLIB) -o trk2hneus $(FFLAGS)
	rm -f trk2hneus.o

l_l_r2llr : l_l_r2llr.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) l_l_r2llr.f  $(SLIB) -o l_l_r2llr $(FFLAGS)
	rm -f l_l_r2llr.o

l_l_r2llrs : l_l_r2llrs.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) l_l_r2llrs.f  $(SLIB) -o l_l_r2llrs $(FFLAGS)
	rm -f l_l_r2llrs.o

l_l_r2llrs_panga : l_l_r2llrs_panga.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) l_l_r2llrs_panga.f  $(SLIB) -o l_l_r2llrs_panga $(FFLAGS)
	rm -f l_l_r2llrs_panga.o

pbo2sios : pbo2sios.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) pbo2sios.f  $(SLIB) -o pbo2sios $(FFLAGS)
	rm -f pbo2sios.o

pbo2cats : pbo2cats.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) pbo2cats.f  $(SLIB) -o pbo2cats $(FFLAGS)
	rm -f pbo2cats.o

pos2neu : pos2neu.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) pos2neu.f  $(SLIB) -o pos2neu $(FFLAGS)
	rm -f pos2neu.o

pos2qmap : pos2qmap.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) pos2qmap.f  $(SLIB) -o pos2qmap $(FFLAGS)
	rm -f pos2qmap.o

qocaload2cats : qocaload2cats.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) qocaload2cats.f  $(SLIB) -o qocaload2cats $(FFLAGS)
	rm -f qocaload2cats.o

qocaload2sios : qocaload2sios.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) qocaload2sios.f  $(SLIB) -o qocaload2sios $(FFLAGS)
	rm -f qocaload2sios.o

scec_csv2sio : scec_csv2sio.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) scec_csv2sio.f  $(SLIB) -o scec_csv2sio $(FFLAGS)
	rm -f scec_csv2sio.o

sio2cats : sio2cats.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) sio2cats.f  $(SLIB) -o sio2cats $(FFLAGS)
	rm -f sio2cats.o

sio2otr : sio2otr.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) sio2otr.f  $(SLIB) -o sio2otr $(FFLAGS)
	rm -f sio2otr.o

sio2otd : sio2otd.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) sio2otd.f  $(SLIB) -o sio2otd $(FFLAGS)
	rm -f sio2otd.o

sio2otx : sio2otx.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) sio2otx.f  $(SLIB) -o sio2otx $(FFLAGS)
	rm -f sio2otx.o

usgs_ts2sio : usgs_ts2sio.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) usgs_ts2sio.f  $(SLIB) -o usgs_ts2sio $(FFLAGS)
	rm -f usgs_ts2sio.o

cmonoc2sios : cmonoc2sios.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) cmonoc2sios.f  $(SLIB) -o cmonoc2sios $(FFLAGS)
	rm -f cmonoc2sios.o

neu_extract_by_neu : neu_extract_by_neu.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) neu_extract_by_neu.f  $(SLIB) -o neu_extract_by_neu $(FFLAGS)
	rm -f neu_extract_by_neu.o

qmap_extract_by_neu : qmap_extract_by_neu.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) qmap_extract_by_neu.f  $(SLIB) -o qmap_extract_by_neu $(FFLAGS)
	rm -f qmap_extract_by_neu.o

tenv3_to_sios : tenv3_to_sios.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) tenv3_to_sios.f  $(SLIB) -o tenv3_to_sios $(FFLAGS)
	rm -f tenv3_to_sios.o

$(GNSS_DATA_FORMAT) : \
	$(GNSS_DATA_FORMAT)(l_l_r2llr_func.o)
	ranlib gnss_data_format.a

$(GNSS_DATA_FORMAT)(l_l_r2llr_func.o): ../../../inc/ftk.h

.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) gnss_data_format $(bindir)
	$(INSTALL) asc2bin_sio $(bindir)
	$(INSTALL) asc2bin_sios $(bindir)
	$(INSTALL) jpl2sios $(bindir)
	$(INSTALL) trk2sios $(bindir)
	$(INSTALL) trk2hneus $(bindir)
	$(INSTALL) l_l_r2llr $(bindir)
	$(INSTALL) l_l_r2llrs $(bindir)
	$(INSTALL) l_l_r2llrs_panga $(bindir)
	$(INSTALL) pbo2sios $(bindir)
	$(INSTALL) pbo2cats $(bindir)
	$(INSTALL) pos2neu $(bindir)
	$(INSTALL) pos2qmap $(bindir)
	$(INSTALL) qocaload2cats $(bindir)
	$(INSTALL) qocaload2sios $(bindir)
	$(INSTALL) scec_csv2sio $(bindir)
	$(INSTALL) sio2cats $(bindir)
	$(INSTALL) sio2otr $(bindir)
	$(INSTALL) sio2otd $(bindir)
	$(INSTALL) sio2otx $(bindir)
	$(INSTALL) usgs_ts2sio $(bindir)
	$(INSTALL) cmonoc2sios $(bindir)
	$(INSTALL) neu_extract_by_neu $(bindir)
	$(INSTALL) qmap_extract_by_neu $(bindir)
	$(INSTALL) tenv3_to_sios $(bindir)

uninstall:
	rm -f $(bindir)/gnss_data_format
	rm -f $(bindir)/asc2bin_sio
	rm -f $(bindir)/asc2bin_sios
	rm -f $(bindir)/jpl2sios
	rm -f $(bindir)/trk2sios
	rm -f $(bindir)/trk2hneus
	rm -f $(bindir)/l_l_r2llr
	rm -f $(bindir)/l_l_r2llrs
	rm -f $(bindir)/l_l_r2llrs_panga
	rm -f $(bindir)/pbo2sios
	rm -f $(bindir)/pbo2cats
	rm -f $(bindir)/pos2neu
	rm -f $(bindir)/pos2qmap
	rm -f $(bindir)/qocaload2cats
	rm -f $(bindir)/qocaload2sios
	rm -f $(bindir)/scec_csv2sio
	rm -f $(bindir)/sio2cats
	rm -f $(bindir)/sio2otr
	rm -f $(bindir)/sio2otd
	rm -f $(bindir)/sio2otx
	rm -f $(bindir)/usgs_ts2sio
	rm -f $(bindir)/cmonoc2sios
	rm -f $(bindir)/neu_extract_by_neu
	rm -f $(bindir)/qmap_extract_by_neu
	rm -f $(bindir)/tenv3_to_sios

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core NONE asc2bin_sio asc2bin_sios jpl2sios trk2sios trk2hneus l_l_r2llr l_l_r2llrs l_l_r2llrs_panga pbo2sios pbo2cats pos2neu pos2qmap qocaload2cats qocaload2sios scec_csv2sio sio2cats sio2otr sio2otd sio2otx usgs_ts2sio cmonoc2sios neu_extract_by_neu qmap_extract_by_neu tenv3_to_sios  NONE asc2bin_sio asc2bin_sios jpl2sios trk2sios trk2hneus l_l_r2llr l_l_r2llrs l_l_r2llrs_panga pbo2sios pbo2cats pos2neu pos2qmap qocaload2cats qocaload2sios scec_csv2sio sio2cats sio2otr sio2otd sio2otx usgs_ts2sio cmonoc2sios neu_extract_by_neu qmap_extract_by_neu tenv3_to_sios
# End of Makefile
