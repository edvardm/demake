.DEFAULT_GOAL = foo

foo:
	echo hello

bar:
	-echo bar


uglyrule:
ifneq (,$(findstring t,$(MAKEFLAGS)))
        +touch archive.a
else
        -touch archive.a
endif
