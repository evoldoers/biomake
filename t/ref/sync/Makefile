
all:
	cd local; $(MAKE) -f ../Makefile --sync ../remote C.bar

%.bar: %.foo
	perl -pe 'tr/a-z/A-Z/' $< > $@
