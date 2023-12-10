include ../compiler.config

all: ftk_lib dyr_2_yrdoyr ymd_2_dyr dyr_2_ymd min max mean total interval 





FTK_LIB = ftk_lib.a
SLIB = ../external/gamit_globk/gg_lib.a 
X11LIB = 
II = 

ftk_lib : $(FTK_LIB)

dyr_2_yrdoyr : dyr_2_yrdoyr.f  $(SLIB) 
	$(FTN) dyr_2_yrdoyr.f  $(SLIB) -o dyr_2_yrdoyr $(FFLAGS)
	rm -f dyr_2_yrdoyr.o

ymd_2_dyr : ymd_2_dyr.f  $(SLIB) 
	$(FTN) ymd_2_dyr.f  $(SLIB) -o ymd_2_dyr $(FFLAGS)
	rm -f ymd_2_dyr.o

dyr_2_ymd : dyr_2_ymd.f  $(SLIB) 
	$(FTN) dyr_2_ymd.f  $(SLIB) -o dyr_2_ymd $(FFLAGS)
	rm -f dyr_2_ymd.o

min : min.f  $(SLIB) ./../inc/ftk.h
	$(FTN) min.f  $(SLIB) -o min $(FFLAGS)
	rm -f min.o

max : max.f  $(SLIB) ./../inc/ftk.h
	$(FTN) max.f  $(SLIB) -o max $(FFLAGS)
	rm -f max.o

mean : mean.f  $(SLIB) ./../inc/ftk.h
	$(FTN) mean.f  $(SLIB) -o mean $(FFLAGS)
	rm -f mean.o

total : total.f  $(SLIB) ./../inc/ftk.h
	$(FTN) total.f  $(SLIB) -o total $(FFLAGS)
	rm -f total.o

interval : interval.f  $(SLIB) 
	$(FTN) interval.f  $(SLIB) -o interval $(FFLAGS)
	rm -f interval.o

$(FTK_LIB) : \
	$(FTK_LIB)(allr8.o) \
	$(FTK_LIB)(covj_1.o) \
	$(FTK_LIB)(desuffix.o) \
	$(FTK_LIB)(determ.o) \
	$(FTK_LIB)(ffind.o) \
	$(FTK_LIB)(getfilename.o) \
	$(FTK_LIB)(getfileext.o) \
	$(FTK_LIB)(getlun.o) \
	$(FTK_LIB)(getpathname.o) \
	$(FTK_LIB)(heaviside.o) \
	$(FTK_LIB)(intlen.o) \
	$(FTK_LIB)(iqr.o) \
	$(FTK_LIB)(map_2points.o) \
	$(FTK_LIB)(matrix_multiply.o) \
	$(FTK_LIB)(mat_transfill.o) \
	$(FTK_LIB)(matply.o) \
	$(FTK_LIB)(ndoyr.o) \
	$(FTK_LIB)(rawnew.o) \
	$(FTK_LIB)(moment.o) \
	$(FTK_LIB)(path_sep.o) \
	$(FTK_LIB)(set_intersect_d.o) \
	$(FTK_LIB)(set_intersect_s.o) \
	$(FTK_LIB)(smean1r8.o) \
	$(FTK_LIB)(stddev.o) \
	$(FTK_LIB)(strarr_and.o) \
	$(FTK_LIB)(strrep.o) \
	$(FTK_LIB)(strsplit.o) \
	$(FTK_LIB)(strsplit2.o) \
	$(FTK_LIB)(strtrim.o) \
	$(FTK_LIB)(trans_r8.o) \
	$(FTK_LIB)(trimright.o) \
	$(FTK_LIB)(uniq.o) \
	$(FTK_LIB)(uniq_i4.o) \
	$(FTK_LIB)(where.o) \
	$(FTK_LIB)(wheres.o) \
	$(FTK_LIB)(xyz2neu.o) \
	$(FTK_LIB)(ymdhms_to_jd_noy2k.o) \
	$(FTK_LIB)(sort_i4.o) \
	$(FTK_LIB)(sort_r8.o) \
	$(FTK_LIB)(polygon_overlap.o) \
	$(FTK_LIB)(polygon_rasterize.o) \
	$(FTK_LIB)(is_point_inside_polygon.o) \
	$(FTK_LIB)(rot_xy.o) \
	$(FTK_LIB)(point_perp_line.o) \
	$(FTK_LIB)(point_cross_line.o) \
	$(FTK_LIB)(polygon_rasterize2.o) \
	$(FTK_LIB)(is_polygon_overlap_polygon.o) \
	$(FTK_LIB)(is_line_intersect_polygon.o) \
	$(FTK_LIB)(is_nan_r_gfortran.o) \
	$(FTK_LIB)(is_nan_r8_gfortran.o) \
	$(FTK_LIB)(dir_test_gfortran.o)
	ranlib ftk_lib.a

$(FTK_LIB)(moment.o): ../inc/ftk.h

.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o

install:
	$(INSTALL) dyr_2_yrdoyr $(bindir)
	$(INSTALL) ymd_2_dyr $(bindir)
	$(INSTALL) dyr_2_ymd $(bindir)
	$(INSTALL) min $(bindir)
	$(INSTALL) max $(bindir)
	$(INSTALL) mean $(bindir)
	$(INSTALL) total $(bindir)
	$(INSTALL) interval $(bindir)

uninstall:
	rm -f $(bindir)/dyr_2_yrdoyr
	rm -f $(bindir)/ymd_2_dyr
	rm -f $(bindir)/dyr_2_ymd
	rm -f $(bindir)/min
	rm -f $(bindir)/max
	rm -f $(bindir)/mean
	rm -f $(bindir)/total
	rm -f $(bindir)/interval

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core dyr_2_yrdoyr ymd_2_dyr dyr_2_ymd min max mean total interval   
# End of Makefile
