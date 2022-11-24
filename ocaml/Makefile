
MLS_TEST=$(patsubst %.ml, .test/%, $(wildcard *.ml))

.PHONY: test MLS_TEST
test: $(MLS_TEST)

clean:
	rm -f $(MLS_TEST)

.test:
	@mkdir -p $@

.test/%: %.ml | .test
	ocaml $<
	@touch $@


