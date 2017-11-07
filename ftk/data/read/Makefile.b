#FTN = ifort
#FFLAGS = -O3 -Vaxlib -save -zero -u -72 -w95 -w90 -cm -assume byterecl -static 
FTN = f77
#FFLAGS = -O3
#F77 = g77-33 
FFLAGS = -O3 -Wuninitialized -Wunused -Wimplicit -fno-f2c -ffast-math -fno-automatic -fno-backslash -Wno-globals -fno-globals

#CC = icc   
#CFLAGS = -O 

CGPSLIB=../../lib/cgps_lib.a ../../../../gglib/gglib.a


LIB=cgps_data_read.a

cgps_data_read : $(LIB)

$(LIB) : \
	$(LIB)(file_info.o) \
	$(LIB)(data_read_sio.o)\
	$(LIB)(l_l_r_read.o)\
	$(LIB)(read_sio_bin.o)\
	$(LIB)(read_sio_hdr.o)
	ar rv $(LIB) *.o 
	ranlib cgps_data_read.a

.f.a:
	$(F77) $(CGPSLIB) -c $(FFLAGS) $< 
	ar rv $@ *.o
#	rm -f $*.o

clean:
	rm -f *.o *.core *.a
