
# Tests:

# plmake hello.world.baz
# should build the following in `hello.world.baz':
 # Baz!
 # B= WORLD A= HELLO
 # I said Baz!

 # plmake test1
# should build the following in `test1':
 # ABC=abc DEF=abc XYZ= xyz

 # plmake test2 DEF=def
# should build the following in `test2':
 # ABC=abc DEF=def XYZ= xyz

 # plmake test3 ABC=123
# should build the following in `test3':
 # ABC=abc DEF=abc XYZ=123 xyz

FOO = .foo

# this currently does not work quite correctly: variables which expand to strings containing spaces should be recognized as multiple targets
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
