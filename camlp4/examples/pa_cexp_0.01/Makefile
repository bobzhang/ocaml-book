include ./Makefile.template

EXAMPLEDIR=example

all:
	$(MAKE) -C $(SOURCEDIR) $@
	$(MAKE) -C $(LIBDIR) $@

example:
	$(MAKE) -C $(EXAMPLEDIR) $@

clean: ocamlclean
	$(MAKE) -C $(SOURCEDIR) $@
	$(MAKE) -C $(LIBDIR) $@
	$(MAKE) -C $(EXAMPLEDIR) $@






