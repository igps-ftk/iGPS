include ../../../compiler.config

all: gnss_data_offset





GNSS_DATA_OFFSET = gnss_data_offset.a
SLIB = ./gnss_data_offset.a ../read/gnss_data_read.a ../write/gnss_data_write.a ../../../lib/ftk_lib.a ../../../external/gamit_globk/gg_lib.a ../../../external/nr/nr_lib.a 
X11LIB = 
II = 

gnss_data_offset : $(GNSS_DATA_OFFSET)

$(GNSS_DATA_OFFSET) : \
	$(GNSS_DATA_OFFSET)(read_offps.o)
	ranlib gnss_data_offset.a


.c.a:
	$(CC) -c $(CFLAGS) -I$(II) $<
	ar rv $@ *.o
	rm -f $*.o

.f.a:
	$(FTN) -c $(FFLAGS) $<
	ar rv $@ *.o
	rm -f $*.o


install:
	$(INSTALL) gnss_data_offset $(bindir)

uninstall:
	rm -f $(bindir)/gnss_data_offset

clean:
	rm -f *.a *.o *core

cleanall:
	rm -f *.a *.o *core NONE  NONE
# End of Makefile
