### Makefile --- 

## Author: root@Hongbos-MacBook-Air.local
## Version: $Id: Makefile,v 0.0 2012/01/12 23:12:03 root Exp $
## Keywords: 
## X-URL: 


### Makefile ends here

c :
	cc -c -I`ocamlc -where` firs.c

on:c
	ocamlopt firs.o call_firs.ml 
ob:c
	ocamlc -custom firs.o call_firs.ml # -custom is necessary
clean:
	rm -rf *~ *.cmi *.o *.out

inspecto:inspect
	ocamlopt inspect.o call_inspect.ml

inspectb:inspect
	ocamlc -custom inspect.o call_inspect.ml

inspect:
	cc -c -I`ocamlc -where` inspect.c

exp:
	cc -c -E -I`ocamlc -where` c_int.c

cint:
	cc -c -I`ocamlc -where` c_int.c

cinto:cint
	ocamlopt c_int.o call_c_int.ml
cintbyte:cint
	ocamlc -custom c_int.o call_c_int.ml

cintbd:clean cintd
	ocamlc -custom -g c_int.o call_c_int.ml
cintd:
	gcc -c -I`ocamlc -where` -g c_int.c

createb:
	cc -c -I`ocamlc -where` create.c
	ocamlc -custom create.o call_create.ml


