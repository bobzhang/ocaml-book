### Makefile --- 

## Author: bobzhang1988@vpl190.wlan.library.upenn.edu
## Version: $Id: Makefile,v 0.0 2012/02/04 16:02:18 bobzhang1988 Exp $
## Keywords: 
## X-URL: 

## make -p -f /dev/null
## for predefining rules 

## OCAMLLIBPATH :=  $(shell ocamlc -where)
## call shell command
### Makefile ends here

### OUT_OBJS=$(addprefix $(OUT_DIR)/,$(OBJS))
### $(OB) $(basename $@).inferred.mli
### cp $(LIB)/$(basename $@).inferred.mli $@

## call built-in Makefile command
# define a suffix rule for .c -> .o
# A good reference 
# .c$(LIB)/*.o :
# 	test -d $(LIB) || mkdir $(LIB)	
# 	$(CC) $(CFLAGS) -c $< -o $(LIB)/$@



.SUFFIXES:
# clear out all suffixes

## C Part
CC = gcc 
OCAMLLIBPATH :=  $(shell ocamlc -where)
CFLAGS	= -g -Wall -I $(OCAMLLIBPATH)
LD	= $(CC)
LDFLAGS	=
RM	= rm
LIB     := _build
SRCS	:= 


# list only those we use
.SUFFIXES: .o .c

.c.o :
	$(CC) $(CFLAGS) -c $< 


## OCAML PART
OCAML = ocaml
.SUFFIXES: .mli .ml .cmi .byte .native .cmo 

OB := ocamlbuild
.ml.mli:
	rm -f $@
	$(OB) $(basename $@).inferred.mli
	cp $(LIB)/$(basename $@).inferred.mli $@
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

## Common PART
clean :
	-$(RM) -rf $(EXE) $(OBJS) $(LIB)/$(EXE) $(LIB)/$(OBJS) \
	*.o *.cmo $(LBI)/*.o $(LIB)/*.cmo
	$(OB) -clean

EXE	:= 
OBJS	:= ${SRCS:.c=.o}

all : $(EXE)
$(EXE) : $(OBJS)
	$(LD) -o $@ $(OBJS)


# _build/single_write.o: single_write.o
# 	test -d $(LIB) || mkdir $(LIB)
# 	cp single_write.o $(LIB)
# # tag single_write.o precious
# write.cma:  _build/single_write.o write.cmo
# 	cd $(LIB); ocamlc -custom -a -o single_write.o write.cmo
