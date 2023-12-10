include ../../compiler.config

all: map_distance sar_intf_tab_2_psxy esa_s1_tiff_EOFs sar_sbas_tab_from_png sar_cal_sat_incidence





SLIB = ../../gnss/data/read/gnss_data_read.a ../../gnss/data/site/gnss_data_site.a ../../lib/ftk_lib.a ../../external/gamit_globk/gg_lib.a 
X11LIB = 
II = 

map_distance : map_distance.f  $(SLIB) 
	$(FTN) map_distance.f  $(SLIB) -o map_distance $(FFLAGS)
	rm -f map_distance.o

sar_intf_tab_2_psxy : sar_intf_tab_2_psxy.f  $(SLIB) 
	$(FTN) sar_intf_tab_2_psxy.f  $(SLIB) -o sar_intf_tab_2_psxy $(FFLAGS)
	rm -f sar_intf_tab_2_psxy.o

esa_s1_tiff_EOFs : esa_s1_tiff_EOFs.f  $(SLIB) 
	$(FTN) esa_s1_tiff_EOFs.f  $(SLIB) -o esa_s1_tiff_EOFs $(FFLAGS)
	rm -f esa_s1_tiff_EOFs.o

sar_sbas_tab_from_png : sar_sbas_tab_from_png.f  $(SLIB) 
	$(FTN) sar_sbas_tab_from_png.f  $(SLIB) -o sar_sbas_tab_from_png $(FFLAGS)
	rm -f sar_sbas_tab_from_png.o

sar_cal_sat_incidence : sar_cal_sat_incidence.f  $(SLIB) 
	$(FTN) sar_cal_sat_incidence.f  $(SLIB) -o sar_cal_sat_incidence $(FFLAGS)
	rm -f sar_cal_sat_incidence.o


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) map_distance $(bindir)
	$(INSTALL) sar_intf_tab_2_psxy $(bindir)
	$(INSTALL) esa_s1_tiff_EOFs $(bindir)
	$(INSTALL) sar_sbas_tab_from_png $(bindir)
	$(INSTALL) sar_cal_sat_incidence $(bindir)

uninstall:
	rm -f $(bindir)/map_distance
	rm -f $(bindir)/sar_intf_tab_2_psxy
	rm -f $(bindir)/esa_s1_tiff_EOFs
	rm -f $(bindir)/sar_sbas_tab_from_png
	rm -f $(bindir)/sar_cal_sat_incidence

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core map_distance sar_intf_tab_2_psxy esa_s1_tiff_EOFs sar_sbas_tab_from_png sar_cal_sat_incidence  map_distance sar_intf_tab_2_psxy esa_s1_tiff_EOFs sar_sbas_tab_from_png sar_cal_sat_incidence
# End of Makefile
