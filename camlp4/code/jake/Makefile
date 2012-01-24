### Makefile --- 

## Author: bobzhang1988@seas758.wireless-pennnet.upenn.edu
## Version: $Id: Makefile,v 0.0 2012/01/23 23:56:42 bobzhang1988 Exp $
## Keywords: 
## X-URL: 


### Makefile ends here

pa_filter.cma: pa_filter.cmo
	cd _build/; ocamlfind ocamlc -a -package batteries pa_filter.cmo batPervasives.cmo -o pa_filter.cma

pa_filter.cmo:
	ocamlbuild pa_filter.cmo

clean:
	ocamlbuild -clean
