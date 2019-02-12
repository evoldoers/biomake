
all:
	cd local; $(MAKE) -f ../Makefile --sync ../remote C.foo

%.bar: %.foo
	perl -pe 'tr/a-z/A-Z/' $< > $@
