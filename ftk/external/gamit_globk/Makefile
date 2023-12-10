include ../../compiler.config

all: gg_lib nblen doy





GG_LIB = gg_lib.a
#SLIB = ../gglib/gglib.a 
X11LIB = 
II = 

gg_lib : $(GG_LIB)

nblen : nblenp.f  $(GG_LIB) 
	$(FTN) nblenp.f  $(GG_LIB) -o nblen $(FFLAGS)
	rm -f nblen.o

doy : doy.f  $(GG_LIB) 
	$(FTN) doy.f  $(GG_LIB) -o doy $(FFLAGS)
	rm -f doy.o

$(GG_LIB) : \
	$(GG_LIB)(blank.o) \
	$(GG_LIB)(casefold.o) \
	$(GG_LIB)(caseunfold.o) \
	$(GG_LIB)(check_ascii.o) \
	$(GG_LIB)(decyrs_to_jd.o) \
	$(GG_LIB)(decyrs_to_ydhms.o) \
	$(GG_LIB)(doygwk.o) \
	$(GG_LIB)(ds2hms.o) \
	$(GG_LIB)(idoy.o) \
	$(GG_LIB)(jd_to_decyrs.o) \
	$(GG_LIB)(jd_to_yds.o) \
	$(GG_LIB)(jd_to_ymdhms.o) \
	$(GG_LIB)(julday.o) \
	$(GG_LIB)(leapyr.o) \
	$(GG_LIB)(lowers.o) \
	$(GG_LIB)(mjd_to_ymdhms.o) \
	$(GG_LIB)(monday.o) \
	$(GG_LIB)(nblen.o) \
	$(GG_LIB)(rcpar.o) \
	$(GG_LIB)(report_stat.o) \
	$(GG_LIB)(sub_char.o) \
	$(GG_LIB)(systime.o) \
	$(GG_LIB)(trimlead.o) \
	$(GG_LIB)(trimlen.o) \
	$(GG_LIB)(uppers.o) \
	$(GG_LIB)(yds_to_jd.o) \
	$(GG_LIB)(ymdhms_to_jd.o) \
	$(GG_LIB)(ymdhms_to_mjd.o) \
	$(GG_LIB)(ymd_to_doy.o)
	ranlib gg_lib.a

.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o

install:
	$(INSTALL) nblen $(bindir)
	$(INSTALL) doy $(bindir)

uninstall:
	rm -f $(bindir)/nblen
	rm -f $(bindir)/doy

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core nblen doy  
# End of Makefile