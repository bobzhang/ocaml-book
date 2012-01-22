### Makefile --- 

## Author: bobzhang1988@seas275.wlan.seas.upenn.edu
## Version: $Id: Makefile,v 0.0 2012/01/12 00:35:51 bobzhang1988 Exp $
## Keywords: 
## X-URL: 


### Makefile ends here


all:
	pdflatex -shell-escape master.tex
	pdflatex -shell-escape master.tex
	open master.pdf

clean:
	rm -rf *~ *.log auto/*  _* *.lot *.lof *.toc *.out *.bak *.html