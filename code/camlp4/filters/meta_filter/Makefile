### Makefile --- 

## Author: bobzhang1988@Hongbos-MacBook-Air.local
## Version: $Id: Makefile,v 0.0 2012/01/06 00:54:21 bobzhang1988 Exp $
## Keywords: 
## X-URL: 


### Makefile ends here

OB = ocamlbuild

all:
	make clean
	ocamlbuild meta_filter.cma

install: remove all
	ocamlfind install meta_filter META _build/*.cm[aoxi]

remove:
	ocamlfind remove meta_filter

clean:
	ocamlbuild -clean

doc:
	$(OB) foo.docdir/index.html
	$(OB) foo.docdir/gen_printer.dot

.PHONY: clean all test doc




%.mli : %.ml
	rm -f $@
	$(OB) $(basename $@).inferred.mli
	cp _build/$(basename $@).inferred.mli $@