include ../../compiler.config

# Makefile for tdefnode, modify for your compilation
FC = gfortran
#FFLAGS=  -gdwarf-2 -w -mcmodel=medium
#FFLAGS=   -mcmodel=medium -w
#FFLAGS=  -w
# use "gcc" to compile source files.
CC = gcc
# the linker is also "gcc". It might be something else with other compilers.
LD = gfortran
# Compiler flags go here.
#CFLAGS =    
# Linker flags go here. 
#LDFLAGS =  
#LDFLAGS =  
# use this command to erase files.
RM = /bin/rm -f
# list of generated object files.
OBJS = tdefnode.o tdefavail.o solve1.o writegps.o rwparms.o
# program executable file name.
PROG = tdefnode

# top-level rule, to compile everything.
all: $(PROG)

# rule to link the program
$(PROG): $(OBJS)
	$(LD) $(LDFLAGS) $(OBJS) -o $(PROG)

# rule for file "main.o".
tdefnode.o: tdefnode.f
	$(FC) $(FFLAGS) -c tdefnode.f solve1.f writegps.f rwparms.f

# rule for file "file1.o".
avail.o: tdefavail.c  
	$(CC) $(CFLAGS) -c tdefavail.c

install:
	$(INSTALL) tdefnode $(bindir)

uninstall:
	rm -f $(bindir)/tdefnode

# rule for cleaning re-compilable files.
clean:
	$(RM) $(PROG) $(OBJS)


