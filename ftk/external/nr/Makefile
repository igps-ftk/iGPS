include ../../compiler.config

all: nr_lib





NR_LIB = nr_lib.a
SLIB = 
X11LIB = 
II = 

nr_lib : $(NR_LIB)

$(NR_LIB) : \
	$(NR_LIB)(amoeba.o) \
	$(NR_LIB)(amotry.o) \
	$(NR_LIB)(avevar.o) \
	$(NR_LIB)(bessj0.o) \
	$(NR_LIB)(betacf.o) \
	$(NR_LIB)(betai.o) \
	$(NR_LIB)(covsrt.o) \
	$(NR_LIB)(dsvdcmp.o) \
	$(NR_LIB)(dsvbksb.o) \
	$(NR_LIB)(dpythag.o) \
	$(NR_LIB)(gammln.o) \
	$(NR_LIB)(gasdev.o) \
	$(NR_LIB)(gaussj.o) \
	$(NR_LIB)(lfit.o) \
	$(NR_LIB)(dludcmp.o) \
	$(NR_LIB)(period.o) \
	$(NR_LIB)(qrdcmp.o) \
	$(NR_LIB)(qrsolv.o) \
	$(NR_LIB)(ran1.o) \
	$(NR_LIB)(scargle.o) \
	$(NR_LIB)(sort.o) \
	$(NR_LIB)(sortm.o) \
	$(NR_LIB)(spline.o) \
	$(NR_LIB)(splint.o) \
	$(NR_LIB)(spline1.o) \
	$(NR_LIB)(splint1.o) \
	$(NR_LIB)(svdcmp.o) \
	$(NR_LIB)(svbksb.o)
	ranlib nr_lib.a


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:

uninstall:

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core NONE  NONE
# End of Makefile
