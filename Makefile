MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
MAKEFILE_DIR := $(dir $(MAKEFILE_PATH))
SWIPL = $(MAKEFILE_DIR)bin/swipl_wrap

prefix = /usr/local
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin

all: clean test

test:
	$(SWIPL) -q -t test -l prolog/test/test

test-%:
	$(SWIPL) -q -t "test($*)" -l prolog/test/test

clean:
	git clean -fd t/target

install:
	ln -sf $(MAKEFILE_DIR)bin/biomake $(bindir)/biomake
