all: cgps_cme_corr.a ts_corr_mat 

F77 = f77
FFLAGS =  -O3# -Wuninitialized -Wno-globals -Wunused -Wimplicit -fno-f2c -ffast-math -fno-automatic -fno-backslash -finit-local-zero -fno-globals

#F77 = g77-33 
#FFLAGS = -O3 -Wuninitialized -Wunused -Wimplicit -fno-f2c -ffast-math -fno-automatic -fno-backslash -Wno-globals -fno-globals

LIB=cgps_cme_corr.a

SLIB= ../../../../gglib/gglib.a ./cgps_cme_corr.a ../../data/site/cgps_data_site.a ../../data/read/cgps_data_read.a ../../data/write/cgps_data_write.a ../../lib/cgps_lib.a  

ts_corr_mat: 
	$(F77) ts_corr_mat.f $(SLIB) -o ts_corr_mat

$(LIB): \
	$(LIB)(ts_corr.o)
	ar rv cgps_cme_corr.a *.o
	ranlib $(LIB)
	rm -f *.o

.f.a:
	$(F77) -c $(FFLAGS) $<
	#ar rv $(LIb) $*.o

clean:
	rm -f $(OBJS) *core ts_corr_mat cgps_cme_corr.a
 
