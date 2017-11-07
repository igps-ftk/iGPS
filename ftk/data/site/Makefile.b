all: cgps_data_site.a test_site_read 

F77 = f77
FFLAGS =  -O3# -Wuninitialized -Wno-globals -Wunused -Wimplicit -fno-f2c -ffast-math -fno-automatic -fno-backslash -finit-local-zero -fno-globals

#F77 = g77-33 
#FFLAGS = -O3 -Wuninitialized -Wunused -Wimplicit -fno-f2c -ffast-math -fno-automatic -fno-backslash -Wno-globals -fno-globals

LIB=cgps_data_site.a

#SLIB= ../../../../gglib/gglib.a ../../lib/cgps_lib.a  
SLIB=../../lib/cgps_lib.a ../../../../gglib/gglib.a


test_site_read: 
#	$(F77) $(SLIB) ./cgps_data_site.a test_site_read.f  -o test_site_read
	$(F77) test_site_read.f ./cgps_data_site.a $(SLIB) -o test_site_read


$(LIB): \
	$(LIB)(site_coords_query.o)\
	$(LIB)(site_read.o)\
	$(LIB)(site_search_byrect.o)
	ar rv cgps_data_site.a *.o
	ranlib $(LIB)
	rm -f *.o

.f.a:
	$(F77) -c $(FFLAGS) $< 
	#ar rv $(LIb) $*.o

clean:
	rm -f *.o *core cgps_data_site.a test_site_read 
