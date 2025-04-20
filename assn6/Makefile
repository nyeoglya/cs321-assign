CAMLC	 	  = ocamlc
CAMLOPT		= ocamlopt
CAMLCFLAG = -thread 
CAMLDEP=ocamldep

CAMLYACC=ocamlyacc
YACCFLAGS=-v
CAMLLEX=ocamllex

PARSER = parser.ml parser.mli lexer.ml
TARGET = hw6
LIB = lib.cma
OBJ = tml.cmo heap.cmo parser.cmo lexer.cmo inout.cmo eval.cmo loop.cmo

all: $(OBJ) hw6.cmo
	$(CAMLC) -o lib.cma -a $(OBJ)
	$(CAMLC) -o $(TARGET) lib.cma hw6.cmo 

clean:
	rm -f $(TARGET) $(PARSER) $(LIB) *.annot *.aux *.log *.cm[iox] *.dvi *~ .*~ #*# 

.SUFFIXES : .ml .mli .cmo .cmi .cmx .mll .mly 

.ml.cmo: $<
	$(CAMLC) $(CAMLCFLAG) -c $< -o $@
.ml.cmx: $<
	$(CAMLOPT) $(CAMLCFLAG) -c $< -o $@
.mli.cmi: $<
	$(CAMLC) $(CAMLCFLAG) -c $< -o $@
.mll.cmo:
	$(CAMLLEX) $<
	$(CAMLC) -c $*.ml
.mll.cmx:
	$(CAMLLEX) $<
	$(CAMLOPT) -c $*.ml
.mly.cmo:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli
	$(CAMLC) -c $*.ml
.mly.cmx:
	$(CAMLYACC) $<
	$(CAMLOPT) -c $*.mli
	$(CAMLOPT) -c $*.ml
.mly.cmi:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli
.mll.ml:
	$(CAMLLEX) $<
.mly.ml:
	$(CAMLYACC) $<
	
depend: 
	$(CAMLDEP) *.mli *.ml > .depend 

.PHONY: depend

include .depend

