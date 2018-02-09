#
# Polygen
# src-level makefile
#
# (C) 2002-2018 Alvise Spano'
#

build:
	ocamlbuild -use-ocamlfind -libs unix main.native
	ln -sf main.native polygen

clean:
	ocamlbuild -clean
	
.PHONY: build clean
