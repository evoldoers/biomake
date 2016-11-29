
# Tests

FOO = .foo

%.bar: %$(FOO)
	perl -pe 'tr/a-z/A-Z/' $< > $@

$A.baz: $A.bar
	(echo "Baz!"; cat $<; echo "I said Baz!") > $@

$A.$B.foo:
	echo b= $B a= $A >$@

XYZ := $(ABC) xyz
DEF ?= $(ABC)
ABC = abc

test%:
	echo ABC=$(ABC) DEF=$(DEF) XYZ=$(XYZ) >$@

DEPS := yorks.lancasters.foo starks.lannisters.foo
parallels: $(DEPS)
	cat $(DEPS) >$@

S := hello
S += world
hw.test:
	echo $S >$@
