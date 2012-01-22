### Makefile --- 

## Author: bobzhang1988@seas555.wireless-pennnet.upenn.edu
## Version: $Id: Makefile,v 0.0 2012/01/14 07:01:50 bobzhang1988 Exp $
## Keywords: 
## X-URL: 


### Makefile ends here
## make -j4 parallel build
OUT:=_build
OB:=ocamlbuild
.SUFFIXES:
.SUFFIXES: .mli .ml .cmi .byte .native .cmo 

aparser.ml: ast.cmi
	test $(OUT) || mkdir $(OUT)
	cp aparser.mly $(OUT)
	cd ${OUT} ; menhir --external-tokens Token --explain \
	--infer  aparser.mly


%.native:
	$(OB) $@
%.byte:
	$(OB) $@
%.cmo:
	$(OB) $@
.mli.cmi:
	$(OB) $@
.ml.cmi: 
	$(OB) $@

ulexer.byte:
	$(OB) aparser.ml
	cd $(OUT); rm aparser.mly
	$(OB) ulexer.byte
clean:
	rm -rf aparser.ml aparser.mli *.o _build/ *.byte *.native


all: clean 