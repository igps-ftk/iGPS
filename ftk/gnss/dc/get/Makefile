include ../../../compiler.config

all: sites_minus rdhold gnss_dc_get_lib get_met get_nav get_orb get_clk get_erp get_rnx get_rnx_igs.org.cn get_hfile rdgsac





GNSS_DC_GET_LIB = gnss_dc_get_lib.a
SLIB = ../../data/site/gnss_data_site.a ../../../lib/ftk_lib.a ../../../external/gamit_globk/gg_lib.a ./gnss_dc_get_lib.a 
X11LIB = 
II = 

sites_minus : sites_minus.f  $(SLIB) 
	$(FTN) sites_minus.f  $(SLIB) -o sites_minus $(FFLAGS)
	rm -f sites_minus.o

rdhold : rdhold.f  $(SLIB) 
	$(FTN) rdhold.f  $(SLIB) -o rdhold $(FFLAGS)
	rm -f rdhold.o

gnss_dc_get_lib : $(GNSS_DC_GET_LIB)

get_met : get_met.f  $(SLIB) 
	$(FTN) get_met.f  $(SLIB) -o get_met $(FFLAGS)
	rm -f get_met.o

get_nav : get_nav.f  $(SLIB) 
	$(FTN) get_nav.f  $(SLIB) -o get_nav $(FFLAGS)
	rm -f get_nav.o

get_orb : get_orb.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) get_orb.f  $(SLIB) -o get_orb $(FFLAGS)
	rm -f get_orb.o

get_clk : get_clk.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) get_clk.f  $(SLIB) -o get_clk $(FFLAGS)
	rm -f get_clk.o

get_erp : get_erp.f  $(SLIB) ../../../inc/ftk.h
	$(FTN) get_erp.f  $(SLIB) -o get_erp $(FFLAGS)
	rm -f get_erp.o

get_rnx : get_rnx.f  $(SLIB) 
	$(FTN) get_rnx.f  $(SLIB) -o get_rnx $(FFLAGS)
	rm -f get_rnx.o

get_rnx_igs.org.cn : get_rnx_igs.org.cn.f  $(SLIB) 
	$(FTN) get_rnx_igs.org.cn.f  $(SLIB) -o get_rnx_igs.org.cn $(FFLAGS)
	rm -f get_rnx_igs.org.cn.o

get_hfile : get_hfile.f  $(SLIB) 
	$(FTN) get_hfile.f  $(SLIB) -o get_hfile $(FFLAGS)
	rm -f get_hfile.o

rdgsac : rdgsac.f  $(SLIB) 
	$(FTN) rdgsac.f  $(SLIB) -o rdgsac $(FFLAGS)
	rm -f rdgsac.o

$(GNSS_DC_GET_LIB) : \
	$(GNSS_DC_GET_LIB)(query_gsac_dhf.o) \
	$(GNSS_DC_GET_LIB)(query_rnx_status.o)
	ranlib gnss_dc_get_lib.a


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) sites_minus $(bindir)
	$(INSTALL) rdhold $(bindir)
	$(INSTALL) get_met $(bindir)
	$(INSTALL) get_nav $(bindir)
	$(INSTALL) get_orb $(bindir)
	$(INSTALL) get_clk $(bindir)
	$(INSTALL) get_erp $(bindir)
	$(INSTALL) get_rnx $(bindir)
	$(INSTALL) get_rnx_igs.org.cn $(bindir)
	$(INSTALL) get_hfile $(bindir)
	$(INSTALL) rdgsac $(bindir)

uninstall:
	rm -f $(bindir)/sites_minus
	rm -f $(bindir)/rdhold
	rm -f $(bindir)/get_met
	rm -f $(bindir)/get_nav
	rm -f $(bindir)/get_orb
	rm -f $(bindir)/get_clk
	rm -f $(bindir)/get_erp
	rm -f $(bindir)/get_rnx
	rm -f $(bindir)/get_rnx_igs.org.cn
	rm -f $(bindir)/get_hfile
	rm -f $(bindir)/rdgsac

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core sites_minus rdhold NONE get_met get_nav get_orb get_clk get_erp get_rnx get_rnx_igs.org.cn get_hfile rdgsac  sites_minus rdhold NONE get_met get_nav get_orb get_clk get_erp get_rnx get_rnx_igs.org.cn get_hfile rdgsac
# End of Makefile
