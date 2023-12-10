include ../../../compiler.config

all: gnss_data_write





GNSS_DATA_WRITE = gnss_data_write.a
SLIB = ../../../lib/ftk_lib.a ../../../external/gamit_globk/gg_lib.a 
X11LIB = 
II = 

gnss_data_write : $(GNSS_DATA_WRITE)

$(GNSS_DATA_WRITE) : \
	$(GNSS_DATA_WRITE)(write_qob.o) \
	$(GNSS_DATA_WRITE)(write_sio.o) \
	$(GNSS_DATA_WRITE)(write_sio_bin.o) \
	$(GNSS_DATA_WRITE)(write_sio_hdr.o)
	ranlib gnss_data_write.a

$(GNSS_DATA_WRITE)(write_qob.o): ../../../inc/ftk.h
$(GNSS_DATA_WRITE)(write_sio.o): ../../../inc/ftk.h

.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) gnss_data_write $(bindir)

uninstall:
	rm -f $(bindir)/gnss_data_write

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core NONE  NONE
# End of Makefile
