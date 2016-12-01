SWIPL = swipl
test:
	$(SWIPL) -t test -l prolog/test/test

clean:
	rm t/target/[a-z]*
