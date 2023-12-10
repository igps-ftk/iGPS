include ../../../compiler.config

all: chk_rnx_local chk_dc_hold chk_rnx_d glorg_origin hold_rnx_stat lhl chk_pbo_hold





SLIB = ../../data/read/gnss_data_read.a ../../data/site/gnss_data_site.a ../../data/write/gnss_data_write.a ../../../lib/ftk_lib.a ../../../external/gamit_globk/gg_lib.a 
X11LIB = 
II = 

chk_rnx_local : chk_rnx_local.f  $(SLIB) 
	$(FTN) chk_rnx_local.f  $(SLIB) -o chk_rnx_local $(FFLAGS)
	rm -f chk_rnx_local.o

chk_dc_hold : chk_dc_hold.f  $(SLIB) 
	$(FTN) chk_dc_hold.f  $(SLIB) -o chk_dc_hold $(FFLAGS)
	rm -f chk_dc_hold.o

chk_rnx_d : chk_rnx_d.c  $(SLIB) 
	$(CC)  chk_rnx_d.c $(CFLAGS) -o chk_rnx_d
	rm -f chk_rnx_d.o

glorg_origin : glorg_origin.f  $(SLIB) 
	$(FTN) glorg_origin.f  $(SLIB) -o glorg_origin $(FFLAGS)
	rm -f glorg_origin.o

hold_rnx_stat : hold_rnx_stat.f  $(SLIB) 
	$(FTN) hold_rnx_stat.f  $(SLIB) -o hold_rnx_stat $(FFLAGS)
	rm -f hold_rnx_stat.o

lhl : lhl.f  $(SLIB) 
	$(FTN) lhl.f  $(SLIB) -o lhl $(FFLAGS)
	rm -f lhl.o

chk_pbo_hold : chk_pbo_hold.f  $(SLIB) 
	$(FTN) chk_pbo_hold.f  $(SLIB) -o chk_pbo_hold $(FFLAGS)
	rm -f chk_pbo_hold.o


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) chk_rnx_local $(bindir)
	$(INSTALL) chk_dc_hold $(bindir)
	$(INSTALL) chk_rnx_d $(bindir)
	$(INSTALL) glorg_origin $(bindir)
	$(INSTALL) hold_rnx_stat $(bindir)
	$(INSTALL) lhl $(bindir)
	$(INSTALL) chk_pbo_hold $(bindir)

uninstall:
	rm -f $(bindir)/chk_rnx_local
	rm -f $(bindir)/chk_dc_hold
	rm -f $(bindir)/chk_rnx_d
	rm -f $(bindir)/glorg_origin
	rm -f $(bindir)/hold_rnx_stat
	rm -f $(bindir)/lhl
	rm -f $(bindir)/chk_pbo_hold

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core chk_rnx_local chk_dc_hold chk_rnx_d glorg_origin hold_rnx_stat lhl chk_pbo_hold  chk_rnx_local chk_dc_hold chk_rnx_d glorg_origin hold_rnx_stat lhl chk_pbo_hold
# End of Makefile
