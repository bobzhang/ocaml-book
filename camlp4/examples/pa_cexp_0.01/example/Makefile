include ../Makefile.template

INCLUDES = -I ../$(LIBDIR)
CAMLP4PARAM = -pp "camlp4of ../$(SOURCEDIR)/pa_cexp.cmo"

example: all
all: maybe_test extStream_test

%_test: ../$(LIBDIR)/%.cmo %_test.cmo
	$(OCAMLC) -o $@ $^

clean: ocamlclean
	$(RM) *_test

