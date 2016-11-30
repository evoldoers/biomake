
# Tests

FOO = .foo

%.bar: %$(FOO)
	perl -pe 'tr/a-z/A-Z/' $< > $@

$A.baz: $A.bar
	echo "Baz!" >$@
	cat $< >>$@
	echo "I said Baz!" >> $@

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

one_line: ; echo Recipe on one line >$@

one_line_with_deps: one_line; cp $< $@

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
# note this also tests a variable nested inside a command
# and a comment in a rule block (i.e. this comment)
	echo $(wordlist 3,4,$(FEAR)) >$@

subst:
	echo $(subst bcd,xyz,abcdefg bcd) >$@

patsubst:
	echo $(patsubst %.c,%.o,a.c b.d e.c f.c) >$@

substref:
	echo $(FEAR:killer=thriller) >$@

strip:
	echo $(strip this   string    has    lots      of redundant      space) >$@

findstring1:
	echo $(findstring rabbit,fox cow rabbit duck) >$@

findstring2:
	echo $(findstring rabbit,fox cow duck) >$@

filter:
	echo $(filter %.c,rabbit.c fox.h cow.c duck.cpp) >$@

filter_out:
	echo $(filter-out %.c,rabbit.c fox.h cow.c duck.cpp) >$@

sort:
	echo $(sort rabbit cow duck fox duck elephant) >$@

dir:
	echo $(dir a/bc/def/g) >$@

notdir:
	echo $(notdir a/bc/def/g) >$@

basename:
	echo $(basename punk.rock classical.music thrash.metal) >$@

suffix:
	echo $(suffix punk.rock classical.music thrash.metal) >$@

addprefix:
	echo $(addprefix soap ,dish opera bar) >$@

addsuffix:
	echo $(addsuffix cake,fruit pan pat-a-) >$@

join:
	echo $(join inter super D,national star J) >$@

