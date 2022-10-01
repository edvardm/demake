BIN := myfile

.PHONY: clean
clean:
	rm -rf $(BIN)

foo: opts ?= --bar
foo: foo.c
	cc -o $@ $<
