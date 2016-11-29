
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

silent:
	@echo silent >$@

S := hello
S += world
hw.test:
	echo $S >$@

firstword:
	echo $(firstword $S) >$@

lastword:
	echo $(lastword $S) >$@

word2:
	echo $(word 2,$S) >$@

words:
	echo $(words $S) >$@

FEAR = Fear is the mind killer
wordlist:
	echo $(wordlist 3,4,$(FEAR)) >$@

subst:
	echo $(subst bcd,xyz,abcdefg bcd) >$@
