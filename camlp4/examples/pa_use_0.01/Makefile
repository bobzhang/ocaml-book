include Makefile.template

pa_use.cmo: pa_use.ml
	$(OCAMLC) -c -I +camlp4 -pp camlp4of $^ -o $@

test: test.ml pa_use.cmo 
	$(OCAMLC) -I +threads unix.cma threads.cma -pp "camlp4of pa_use.cmo" test.ml -o $@

all: pa_use.cmo

clean: ocamlclean
	$(RM) test
