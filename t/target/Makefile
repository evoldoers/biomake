
# Tests

simple:
	echo Simple test >simple

target1 target2:
	echo $@ >$@

%.echo:
	echo $* >$@

first_dep: a.echo b.echo c.echo
	echo $< >$@

all_deps: a.echo b.echo c.echo
	echo $^ >$@

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

vars%:
	echo ABC=$(ABC) DEF=$(DEF) XYZ=$(XYZ) >$@

hyphenated-var = This variable is hyphenated
hyphenated_var:
	echo $(hyphenated-var) >$@

unbound_var:
	echo $(nonexistent_variable) >$@

escape_dollar:
	echo $$ >$@

DEPS := yorks.lancasters.foo starks.lannisters.foo
multi_targets_from_var: $(DEPS)
	cat $(DEPS) >$@

silent:
	@echo silent >$@

one_line: ; echo Recipe on one line >$@

one_line_with_deps: one_line; cp $< $@

S := hello
S += world
append_var:
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

wildcard:
	echo $(wildcard ../dir/*) >$@

abspath:
	echo $(abspath /usr/share/../lib) >$@
	echo $(abspath /etc//hosts) >>$@
	echo $(abspath /this/file/does/not/exist) >>$@

realpath:
	echo $(realpath /this/file/does/not/exist) >$@

x1 = y1
y1 = z
a1 := $($(x1))
computed_var1:
	echo $(a1) >$@

x2 = y2
y2 = z2
z2 = u
a2 := $($($(x2)))
computed_var2:
	echo $(a2) >$@

x3 = $(y3)
y3 = z3
z3 = Hello
a3 := $($(x3))
computed_var3:
	echo $(a3) >$@

bar2lines = Bar
define two-lines =
echo foo >$@
echo $(bar2lines) >$@
endef

test_at = echo hello >$@
test_at:
	$(test_at)

two_lines:
	$(two-lines)

FUNC = echo $1 is $2
call:
	$(call FUNC,chris,reekin) >$@
	$(call FUNC,ian,tidy) >>$@

shell:
	echo $(shell echo hello world) >$@

shell_assign != echo hello there
shell_assign:
	echo $(shell_assign) >$@

subdir/target_file:
	echo $(@F) >$@

subdir/target_dir:
	echo $(@D) >$@

%_file.txt:
	echo $(*F) >$@

%_dir.txt:
	echo $(*D) >$@

dep_file: subdir/foo
	echo $(<F) >$@

dep_dir: subdir/foo
	echo $(<D) >$@

deps_file: subdir/foo subdir/bar/empty
	echo $(^F) >$@

deps_dir: subdir/foo subdir/bar/empty
	echo $(^D) >$@

forced_rebuild_setup := $(shell touch forced_rebuild)
forced_rebuild: FORCE
	echo test2 >$@

FORCE:
	echo wrong >forced_rebuild

foreach:
	echo $(foreach thing,Life Death Tax,$(thing) is constant.) >$@

bad_function_syntax:
	echo $(non sense) >$@
