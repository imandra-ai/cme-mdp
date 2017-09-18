.PHONY: all build test doc clean opam-switch dev-setup uninstall

all: build test

opam-switch:
	opam switch cme-binary --alias-of 4.05.0

build:
	jbuilder build
