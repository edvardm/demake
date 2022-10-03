# BUILDOPTS := "--fast --haddock-deps"

BUILDOPTS := "--fast"
PROG := "demake"

# generate local hoogle index
gen-local-hoogle:
    stack hoogle -- generate --local

dev-deps:
    # LTS might not contain things we need
    cabal install ghcide hindent ghcid

# prepare project ready for development
dev-init: dev-deps build

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
    stack build {{ BUILDOPTS }} --no-run-tests --file-watch --exec "stack exec {{ PROG }} -- -D test/01.mk"

watch-test:
    stack build {{ BUILDOPTS }} --file-watch --test

# lint all files
lint:
    hlint --git

# run tests
test:
    stack test --fast

# format code
fmt: fmt-f

fmt-f:
    fourmolu --indentation 2 --mode inplace src/**/*.hs app/*.hs test/*.hs

amend-if-changed:
    git status || git --amend --no-edit commit .

# new version
bump kind="minor": fmt amend-if-changed
    bumpver update --{{ kind }}

# List TODOs
todo:
    bat .todo.md

# copy to ~/.local/bin/
install:
    stack install

markdown-filter:
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

mk-readme:
    just markdown-filter < .readme.tmpl.md > README.md
