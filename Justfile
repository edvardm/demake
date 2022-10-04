BUILDOPTS := "-j4 --fast --pedantic"
PROG := "demake"

# generate local hoogle index
gen-local-hoogle:
    stack hoogle -- generate --local

dev-deps:
    # LTS might not contain things we need. Yet this could produce incompatible
    # binaries -- use stack instead with manual overrides?
    just require-cmd ghcide
    just require-cmd hindent
    just require-cmd ghcid

# prepare project ready for development
dev-init: check-ghcup dev-deps build

check-ghcup:
    command -v ghcup > /dev/null || echo "ghcup not found, recommend installing it: https://www.haskell.org/ghcup/install/"

require-cmd cmd:
    @command -v {{cmd}} > /dev/null || ("echo {{cmd}} not found, installing"; cabal v2-install {{cmd}})

# start local hoogle server
hoogle: gen-local-hoogle
    stack hoogle -- server --local --port=8080

# run for given file printing tasks.py to stdout
run file:
    @stack exec {{ PROG }} -- {{ file }} -o-

# build all targets
build:
    stack build {{ BUILDOPTS }}

# watch for changes, run for sample file when complete
watch:
    stack build {{ BUILDOPTS }} --fast --no-run-tests --file-watch --exec "stack exec {{ PROG }} -- -D test/01.mk"

watch-test:
    stack build {{ BUILDOPTS }} --fast --file-watch --test

# lint all files
lint: fmt
    hlint --git

# run stack clean
clean:
    stack clean --full

# run tests
test:
    stack test --fast

# format code
fmt:
    fourmolu --indentation 2 --mode inplace src/**/*.hs app/*.hs test/*.hs
    just --fmt --unstable  # format self

amend-if-changed:
    git status || git --amend --no-edit commit .

# new version
bump kind="minor": fmt amend-if-changed
    bumpver update --{{ kind }}

# List TODOs
todo:
    nvim .todo.md

# copy to ~/.local/bin/
install:
    stack install

# Substitute {! file/path !} with contents of that file
include-file-flt:
    #!/usr/bin/env python
    import sys
    import re
    import pathlib

    pattern = r"\{!\s*([\w/\.-]+)\s*!\}"
    for entry in sys.stdin.readlines():
        if re.match(pattern, entry):
            entry = pathlib.Path(re.sub(pattern, "\g<1>", entry).strip()).read_text()
        sys.stdout.write(entry)

update-examples:
    just run examples/02.mk > examples/02-tasks.py
    git status examples/02-tasks.py || git commit -m "updatetd example 02"

update-readme:
    just include-file-flt < readme.tmpl.md > README.md
