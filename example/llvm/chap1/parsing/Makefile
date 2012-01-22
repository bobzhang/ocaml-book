### Makefile --- 

## Author: bobzhang1988@seas555.wireless-pennnet.upenn.edu
## Version: $Id: Makefile,v 0.0 2012/01/16 02:29:50 bobzhang1988 Exp $
## Keywords: 
## X-URL: 



.SUFFIXES:
.SUFFIXES: .mli .ml .mly .mll .cmi .cmo .cmx .cmm .cmxa .s .S .o .so

EXTS:=mli,ml,mly,mll,cmi,cmo,cmx,cmm,cmxa,s,S,o,so

OCAMLLIBDIR=$(shell ocamlc -v | tail -n -1 | sed -e 's/.* //g' | sed -e 's/\\/\//g' | tr -d '\r')

OCAMLC= ocamlfind ocamlc
OCAMLOPT= ocamlfind ocamlopt
CWD=.
CWD=$(shell pwd)
OB= ocamlbuild
OCAMLFLAGS :=
DEST:=./build

cp:
	@echo $(EXTS)
	-@for file in *.{$(EXTS)}; do\
		echo $$file; \
		cp $$file $(DEST)/$$file;\
  	done;	
		# cp $$(file) `dirname $$file`/DEST/`basename $$file`; \



%.mli : %.ml
	@echo "$(OCAMLC): $< ---> $@ "
	$(OCAMLC) $(OCAMLFLAGS) -i $(CWD)/$< > $@
%.cmi : %.mli
	@echo "$(OCAMLC): $< ---> $@"
	$(OCAMLC) $(OCAMLFLAGS) -c $(CWD)/$<

%.cmo: %.ml
	@echo "$(OCAMLC): $< ---> $@"
	$(OCAMLC) $(OCAMLFLAGS) -c $(CWD)/$<

%.cmx: %.ml
	@echo "$(OCAMLOPT): $< ---> $@"
	$(OCAMLOPT) $(CAMLFLAGS) -c $(CWD)/$<


%.o %.obj: %.c
	@echo "$(OCAMLOPT): $< ---> $@"
	$(CAMLC) $(CAMLFLAGS) -c $(CWD)/$<
clean::
	-$(RM) -r *.cmi *.cmo *.cmx *.cma *.cmxa TAGS tags
	-$(RM) -r *.o core gmon.out *~ .*~
	-$(RM) -r *.obj *.lib *.exp
	-$(RM) -r *.tmp *.bak?.tmp .*.bak?.tmp

aparser:ast.cmi
	menhir --external-tokens Token --infer aparser.mly



### Makefile ends here
