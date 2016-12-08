SWIPL = swipl
all: clean test

test:
	$(SWIPL) -t test -l prolog/test/test

test-%:
	$(SWIPL) -t "test($*)" -l prolog/test/test

clean:
	git clean -fd t/target
