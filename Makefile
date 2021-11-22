
build:
	@dune build @install --profile=release

clean:
	@dune clean

doc:
	@dune build @doc

test:
	@dune runtest --force --no-buffer

ICNF_SOLVE=src/icnf-solve/icnf_solve.exe
icnf-solve: build
	@dune build $(ICNF_SOLVE) --profile=release
	@strip _build/default/$(ICNF_SOLVE)
	@ln -sf _build/default/$(ICNF_SOLVE) .

build-rust-stubs:
	@./build_rust.sh

all: build test

VERSION=$(shell awk '/^version:/ {print $$2}' batsat.opam)
update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" $(wildcard src/*.ml) $(wildcard src/*.mli)
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" $(wildcard src/*.ml) $(wildcard src/*.mli)

reindent:
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 echo "reindenting: "
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 ocp-indent -i

watch:
	@dune build @install --profile=release -w

.PHONY: prebuild check release clean

