F77=g77-34
PROG=rnxdc
SRCS=\
	rnxdc.f
$(PROG):$(SRCS)
	$(F77) $(SRCS) /home/tianyf/gpsf/lib/gg.a -o $(PROG)
	./$(PROG) a.05o a.obs
clean:
	rm $(PROG) 
