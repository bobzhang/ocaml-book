
OB = ocamlbuild
all:
	$(OB) -lflags quotutil.cmo lambda.cma

%.mli : %.ml
	rm -f $@
	$(OB) $(basename $@).inferred.mli
	cp _build/$(basename $@).inferred.mli $@

