-include 01.mk

foo: opts ?= fname --bar --foo
foo:
	cmd $(opts)
