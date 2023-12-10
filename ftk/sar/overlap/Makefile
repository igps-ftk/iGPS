include ../../compiler.config

all: s1_manifest_overlap s1_overlap_check is_point_in_polygon s1_frame_match s1_frame_intersect





SLIB = ../../gnss/data/read/gnss_data_read.a ../../gnss/data/site/gnss_data_site.a ../../lib/ftk_lib.a ../../external/gamit_globk/gg_lib.a 
X11LIB = 
II = 

s1_manifest_overlap : s1_manifest_overlap.f  $(SLIB) 
	$(FTN) s1_manifest_overlap.f  $(SLIB) -o s1_manifest_overlap $(FFLAGS)
	rm -f s1_manifest_overlap.o

s1_overlap_check : s1_overlap_check.f  $(SLIB) 
	$(FTN) s1_overlap_check.f  $(SLIB) -o s1_overlap_check $(FFLAGS)
	rm -f s1_overlap_check.o

is_point_in_polygon : is_point_in_polygon.f  $(SLIB) 
	$(FTN) is_point_in_polygon.f  $(SLIB) -o is_point_in_polygon $(FFLAGS)
	rm -f is_point_in_polygon.o

s1_frame_match : s1_frame_match.f  $(SLIB) ../../inc/ftk.h
	$(FTN) s1_frame_match.f  $(SLIB) -o s1_frame_match $(FFLAGS)
	rm -f s1_frame_match.o

s1_frame_intersect : s1_frame_intersect.f  $(SLIB) 
	$(FTN) s1_frame_intersect.f  $(SLIB) -o s1_frame_intersect $(FFLAGS)
	rm -f s1_frame_intersect.o


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) s1_manifest_overlap $(bindir)
	$(INSTALL) s1_overlap_check $(bindir)
	$(INSTALL) is_point_in_polygon $(bindir)
	$(INSTALL) s1_frame_match $(bindir)
	$(INSTALL) s1_frame_intersect $(bindir)

uninstall:
	rm -f $(bindir)/s1_manifest_overlap
	rm -f $(bindir)/s1_overlap_check
	rm -f $(bindir)/is_point_in_polygon
	rm -f $(bindir)/s1_frame_match
	rm -f $(bindir)/s1_frame_intersect

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core s1_manifest_overlap s1_overlap_check is_point_in_polygon s1_frame_match s1_frame_intersect  s1_manifest_overlap s1_overlap_check is_point_in_polygon s1_frame_match s1_frame_intersect
# End of Makefile
