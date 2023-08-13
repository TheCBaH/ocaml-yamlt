
default: build

build:
	opam exec dune $@

format:
	opam exec dune fmt

utop:
	opam exec dune $@

clean:
	opam exec dune $@

runtest:
	opam exec -- dune $@ --auto-promote

.PHONY: default build clean format top utop
