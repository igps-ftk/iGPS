include ../../../compiler.config

all: test_site_read extract_stinfo rdsit rdsite_old sitcomm sitminus





GNSS_DATA_SITE = gnss_data_site.a
SLIB = ./gnss_data_site.a ../../../lib/ftk_lib.a ../../../external/gamit_globk/gg_lib.a 
X11LIB = 
II = 

test_site_read : test_site_read.f $(GNSS_DATA_SITE) $(SLIB) 
	$(FTN) test_site_read.f $(GNSS_DATA_SITE) $(SLIB) -o test_site_read $(FFLAGS)
	rm -f test_site_read.o

extract_stinfo : extract_stinfo.f  $(SLIB) 
	$(FTN) extract_stinfo.f  $(SLIB) -o extract_stinfo $(FFLAGS)
	rm -f extract_stinfo.o

rdsit : rdsit.f  $(SLIB) 
	$(FTN) rdsit.f  $(SLIB) -o rdsit $(FFLAGS)
	rm -f rdsit.o

rdsite_old : rdsite_old.f  $(SLIB) 
	$(FTN) rdsite_old.f  $(SLIB) -o rdsite_old $(FFLAGS)
	rm -f rdsite_old.o

sitcomm : sitcomm.f  $(SLIB) 
	$(FTN) sitcomm.f  $(SLIB) -o sitcomm $(FFLAGS)
	rm -f sitcomm.o

sitminus : sitminus.f  $(SLIB) 
	$(FTN) sitminus.f  $(SLIB) -o sitminus $(FFLAGS)
	rm -f sitminus.o

$(GNSS_DATA_SITE) : \
	$(GNSS_DATA_SITE)(query_stainfo.o) \
	$(GNSS_DATA_SITE)(rdldsite.o) \
	$(GNSS_DATA_SITE)(read_net.o) \
	$(GNSS_DATA_SITE)(read_neuxyz.o) \
	$(GNSS_DATA_SITE)(site_coords_query.o) \
	$(GNSS_DATA_SITE)(site_read.o) \
	$(GNSS_DATA_SITE)(site_search_byrect.o) \
	$(GNSS_DATA_SITE)(rdsit_.o)
	ranlib gnss_data_site.a


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) test_site_read $(bindir)
	$(INSTALL) extract_stinfo $(bindir)
	$(INSTALL) rdsit $(bindir)
	$(INSTALL) rdsite_old $(bindir)
	$(INSTALL) sitcomm $(bindir)
	$(INSTALL) sitminus $(bindir)

uninstall:
	rm -f $(bindir)/test_site_read
	rm -f $(bindir)/extract_stinfo
	rm -f $(bindir)/rdsit
	rm -f $(bindir)/rdsite_old
	rm -f $(bindir)/sitcomm
	rm -f $(bindir)/sitminus

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core test_site_read extract_stinfo rdsit rdsite_old sitcomm sitminus  test_site_read extract_stinfo rdsit rdsite_old sitcomm sitminus
# End of Makefile
