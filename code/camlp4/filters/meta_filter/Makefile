### Makefile --- 

## Author: bobzhang1988@Hongbos-MacBook-Air.local
## Version: $Id: Makefile,v 0.0 2012/01/06 00:54:21 bobzhang1988 Exp $
## Keywords: 
## X-URL: 


### Makefile ends here


test:
	ocamlbuild test_lift_filter.cmo  
	ocamlbuild test_lift_filter_r.cmo 
all:
	make clean
	ocamlbuild meta_filter.cma

install: remove all
	ocamlfind install meta_filter META _build/*.cm[aoxi]

remove:
	ocamlfind remove meta_filter

clean:
	ocamlbuild -clean

.PHONY: clean all test 


OB = ocamlbuild

%.mli : %.ml
	rm -f $@
	$(OB) $(basename $@).inferred.mli
	cp _build/$(basename $@).inferred.mli $@