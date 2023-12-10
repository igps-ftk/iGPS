include ../../compiler.config

all: sar_los_profile_fit profile_vector_auto sar_los_profile_auto





SAR_PROFILE_LIB = sar_profile_lib.a
SAR_PROFILE_LIB = sar_profile_lib.a
SLIB = ../../gnss/data/read/gnss_data_read.a ../../gnss/data/site/gnss_data_site.a ../../lib/ftk_lib.a ../../external/gamit_globk/gg_lib.a ./sar_profile_lib.a 
X11LIB = 
II = 

sar_los_profile_fit : sar_los_profile_fit.f  $(SLIB) 
	$(FTN) sar_los_profile_fit.f  $(SLIB) -o sar_los_profile_fit $(FFLAGS)
	rm -f sar_los_profile_fit.o

profile_vector_auto : profile_vector_auto.f $(SAR_PROFILE_LIB) $(SLIB) ../../inc/ftk.h
	$(FTN) profile_vector_auto.f $(SAR_PROFILE_LIB) $(SLIB) -o profile_vector_auto $(FFLAGS)
	rm -f profile_vector_auto.o

sar_los_profile_auto : sar_los_profile_auto.f $(SAR_PROFILE_LIB) $(SLIB) ../../inc/ftk.h
	$(FTN) sar_los_profile_auto.f $(SAR_PROFILE_LIB) $(SLIB) -o sar_los_profile_auto $(FFLAGS)
	rm -f sar_los_profile_auto.o

$(SAR_PROFILE_LIB) : \
	$(SAR_PROFILE_LIB)(along_line_shift.o) \
	$(SAR_PROFILE_LIB)(perp_line_shift.o)
	ranlib sar_profile_lib.a

$(SAR_PROFILE_LIB) : \
	$(SAR_PROFILE_LIB)(along_line_shift.o) \
	$(SAR_PROFILE_LIB)(perp_line_shift.o)
	ranlib sar_profile_lib.a


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) sar_los_profile_fit $(bindir)
	$(INSTALL) profile_vector_auto $(bindir)
	$(INSTALL) sar_los_profile_auto $(bindir)

uninstall:
	rm -f $(bindir)/sar_los_profile_fit
	rm -f $(bindir)/profile_vector_auto
	rm -f $(bindir)/sar_los_profile_auto

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core sar_los_profile_fit profile_vector_auto sar_los_profile_auto  sar_los_profile_fit profile_vector_auto sar_los_profile_auto
# End of Makefile
