.PHONY: deps
PROJ := myproj

deps: ## install dependencies
	pip install poetry
	poetry install --no-dev

foo: foo.o
	# some funny comment here
	cc -o $@ $<

bar: dep_a dep_b
	@echo running in $(PROJ)

.PHONY: dep_a
dep_a:
	@echo "produce a"

.PHONY: dep_b
dep_b:
	@echo "produce b"

.PHONY: install
install: deps

foo.o: foo.c
	$(CC) -o $@ $<

foo.c:
	echo "void main(int argc, char **argv[]); { exit(0); }" > $@
