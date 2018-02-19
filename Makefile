MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
MAKEFILE_DIR := $(dir $(MAKEFILE_PATH))
SWIPL = $(MAKEFILE_DIR)bin/swipl_wrap

# override these GNU-recommended variables for a custom install
prefix = /usr/local
datadir = $(prefix)/share
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin

.PHONY: all clean test symlink install uninstall

all: clean test

test:
	$(SWIPL) -q -t test -l prolog/test/test

test-%:
	$(SWIPL) -q -t "test($*)" -l prolog/test/test

clean:
	git clean -fd t/target || true

symlink:
	ln -sf $(MAKEFILE_DIR)bin/biomake $(bindir)/biomake

install:
	mkdir -p $(datadir)
	(test -e $(datadir)/biomake && rm -rf $(datadir)/biomake) || true
	cp -r $(MAKEFILE_DIR) $(datadir)/biomake
	ln -sf $(datadir)/biomake/bin/biomake $(bindir)/biomake

uninstall:
	(test -e $(datadir)/biomake && rm -rf $(datadir)/biomake) || true
	(test -e $(bindir)/biomake && rm -rf $(bindir)/biomake) || true

VERSION = "v0.0.1" 
IM=cmungall/biomake

docker-build:
	@docker build -t $(IM):$(VERSION) . \
	&& docker tag $(IM):$(VERSION) $(IM):latest

docker-run:
	docker run --rm -ti --name biomake $(IM)

docker-publish: build
	@docker push $(IM):$(VERSION) \
	&& docker push $(IM):latest
