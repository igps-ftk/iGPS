include ../../../compiler.config

all: test_reads fdiff





#GNSS_DATA_READ = gnss_data_read.a
GNSS_DATA_READ = gnss_data_read.a
SLIB = ./gnss_data_read.a ../../../lib/ftk_lib.a ../../../external/gamit_globk/gg_lib.a 
X11LIB = 
II = 

test_reads : test_reads.f $(GNSS_DATA_READ) $(SLIB) ../../../inc/ftk.h
	$(FTN) test_reads.f $(GNSS_DATA_READ) $(SLIB) -o test_reads $(FFLAGS)
	rm -f test_reads.o

fdiff : fdiff.f $(GNSS_DATA_READ) $(SLIB) 
	$(FTN) fdiff.f $(GNSS_DATA_READ) $(SLIB) -o fdiff $(FFLAGS)
	rm -f fdiff.o

$(GNSS_DATA_READ) : \
	$(GNSS_DATA_READ)(data_read_sio.o) \
	$(GNSS_DATA_READ)(file_info.o) \
	$(GNSS_DATA_READ)(finfo.o) \
	$(GNSS_DATA_READ)(file_info_usgs.o) \
	$(GNSS_DATA_READ)(query_pbo.o) \
	$(GNSS_DATA_READ)(query_pos.o) \
	$(GNSS_DATA_READ)(read_scec_csv.o) \
	$(GNSS_DATA_READ)(read_cmonoc.o) \
	$(GNSS_DATA_READ)(read_cols.o) \
	$(GNSS_DATA_READ)(read_ctl.o) \
	$(GNSS_DATA_READ)(read_ds607_txt.o) \
	$(GNSS_DATA_READ)(read_grbtxt.o) \
	$(GNSS_DATA_READ)(read_l_l_r.o) \
	$(GNSS_DATA_READ)(read_l_l_r_panga.o) \
	$(GNSS_DATA_READ)(read_load_avg.o) \
	$(GNSS_DATA_READ)(read_load_ref.o) \
	$(GNSS_DATA_READ)(read_met.o) \
	$(GNSS_DATA_READ)(read_sio_bin.o) \
	$(GNSS_DATA_READ)(read_sio_hdr.o) \
	$(GNSS_DATA_READ)(read_pbo.o) \
	$(GNSS_DATA_READ)(read_pos.o) \
	$(GNSS_DATA_READ)(read_sio.o) \
	$(GNSS_DATA_READ)(read_trk.o) \
	$(GNSS_DATA_READ)(l_l_r_read.o) \
	$(GNSS_DATA_READ)(read_poletide_bull_a.o) \
	$(GNSS_DATA_READ)(read_usgs.o) \
	$(GNSS_DATA_READ)(read_qmap.o) \
	$(GNSS_DATA_READ)(read_tenv3.o) \
	$(GNSS_DATA_READ)(read_txt.o) \
	$(GNSS_DATA_READ)(read_psxy.o) \
	$(GNSS_DATA_READ)(read_esa_s1_manifest_safe.o)
	ranlib gnss_data_read.a

$(GNSS_DATA_READ) : \
	$(GNSS_DATA_READ)(data_read_sio.o) \
	$(GNSS_DATA_READ)(file_info.o) \
	$(GNSS_DATA_READ)(finfo.o) \
	$(GNSS_DATA_READ)(file_info_usgs.o) \
	$(GNSS_DATA_READ)(query_pbo.o) \
	$(GNSS_DATA_READ)(query_pos.o) \
	$(GNSS_DATA_READ)(read_scec_csv.o) \
	$(GNSS_DATA_READ)(read_cmonoc.o) \
	$(GNSS_DATA_READ)(read_cols.o) \
	$(GNSS_DATA_READ)(read_ctl.o) \
	$(GNSS_DATA_READ)(read_ds607_txt.o) \
	$(GNSS_DATA_READ)(read_grbtxt.o) \
	$(GNSS_DATA_READ)(read_l_l_r.o) \
	$(GNSS_DATA_READ)(read_l_l_r_panga.o) \
	$(GNSS_DATA_READ)(read_load_avg.o) \
	$(GNSS_DATA_READ)(read_load_ref.o) \
	$(GNSS_DATA_READ)(read_met.o) \
	$(GNSS_DATA_READ)(read_sio_bin.o) \
	$(GNSS_DATA_READ)(read_sio_hdr.o) \
	$(GNSS_DATA_READ)(read_pbo.o) \
	$(GNSS_DATA_READ)(read_pos.o) \
	$(GNSS_DATA_READ)(read_sio.o) \
	$(GNSS_DATA_READ)(read_trk.o) \
	$(GNSS_DATA_READ)(l_l_r_read.o) \
	$(GNSS_DATA_READ)(read_poletide_bull_a.o) \
	$(GNSS_DATA_READ)(read_usgs.o) \
	$(GNSS_DATA_READ)(read_qmap.o) \
	$(GNSS_DATA_READ)(read_tenv3.o) \
	$(GNSS_DATA_READ)(read_txt.o) \
	$(GNSS_DATA_READ)(read_psxy.o) \
	$(GNSS_DATA_READ)(read_esa_s1_manifest_safe.o)
	ranlib gnss_data_read.a

$(GNSS_DATA_READ)(file_info_usgs.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_scec_csv.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_cmonoc.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_cols.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_ds607_txt.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_grbtxt.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_l_l_r.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_l_l_r_panga.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_met.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_sio_bin.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_pbo.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_pos.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_sio.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_trk.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(l_l_r_read.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_poletide_bull_a.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_usgs.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_qmap.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_tenv3.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_psxy.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_esa_s1_manifest_safe.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(file_info_usgs.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_scec_csv.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_cmonoc.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_cols.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_ds607_txt.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_grbtxt.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_l_l_r.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_l_l_r_panga.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_met.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_sio_bin.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_pbo.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_pos.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_sio.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_trk.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(l_l_r_read.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_poletide_bull_a.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_usgs.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_qmap.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_tenv3.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_psxy.o): ../../../inc/ftk.h
$(GNSS_DATA_READ)(read_esa_s1_manifest_safe.o): ../../../inc/ftk.h

.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) test_reads $(bindir)
	$(INSTALL) fdiff $(bindir)

uninstall:
	rm -f $(bindir)/test_reads
	rm -f $(bindir)/fdiff

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core test_reads fdiff  test_reads fdiff
# End of Makefile
