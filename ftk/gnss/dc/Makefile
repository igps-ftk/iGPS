include ../../compiler.config

DIRS	= check   \
	  get
 
LIB	= 

all:	$(LIB)
	for d in $(DIRS); do \
		(cd $$d; $(MAKE) all); \
	done

install:	all
	for d in $(DIRS); do \
		(cd $$d; $(MAKE) install); \
	done

uninstall:	all
	for d in $(DIRS); do \
		(cd $$d; $(MAKE) uninstall); \
	done


clean:
	for d in $(DIRS); do \
		(cd $$d; $(MAKE) clean); \
	done

cleanall:
	for d in $(DIRS); do \
		(cd $$d; $(MAKE) cleanall); \
	done

