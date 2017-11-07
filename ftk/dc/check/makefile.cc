all: chk_rnx_d chk_rnx_dz

CC=cc
CFLAGS=-lz
chk_rnx_d:
	$(CC) -o chk_rnx_d $(CFLAGS) func_crx2rnx.c chk_rnx_d.c
	\rm -f func_crx2rnx.o chk_rnx_d.o

chk_rnx_dz:
	$(CC) -o chk_rnx_dz $(CFLAGS) func_crx2rnx_z.c chk_rnx_dz.c
	\rm -f func_crx2rnx_z.o chk_rnx_dz.o

clean:
	\rm -f chk_rnx_d chk_rnx_dz
