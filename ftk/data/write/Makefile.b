#FTN = ifort
#FFLAGS = -O3 -Vaxlib -save -zero -u -72 -w95 -w90 -cm -assume byterecl -static 
F77 = f77
#FFLAGS = -O3
#F77 = g77-33 
FFLAGS = -O3 -Wuninitialized -Wunused -Wimplicit -fno-f2c -ffast-math -fno-automatic -fno-backslash -Wno-globals -fno-globals

#CC = icc   
#CFLAGS = -O 

CGPSLIB=../../lib/cgps_lib.a ../../../../gglib/gglib.a

LIB=cgps_data_write.a

cgps_data_read : $(LIB)

$(LIB) : \
	$(LIB)(write_sio_bin.o)\
	$(LIB)(write_sio_hdr.o)
	ar rv $(LIB) *.o
	ranlib cgps_data_write.a

.f.a:
	$(F77) $(CGPSLIB) -c $(FFLAGS) $<
	ar rv $@ *.o
#	rm -f $*.o


clean:
	rm -f *.o *.core *.a
