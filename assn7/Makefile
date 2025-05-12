CAMLC	 	  = ocamlc
CAMLOPT		= ocamlopt
CAMLCFLAG = -thread 
CAMLDEP=ocamldep

CAMLYACC=ocamlyacc
YACCFLAGS=-v
CAMLLEX=ocamllex

PARSER = parser.ml parser.mli lexer.ml
TARGET = hw7
OBJ = set_type.cmo dict.cmo mach.cmo ast.cmo parser.cmo lexer.cmo inout.cmo ast_valid.cmo ast_print.cmo core.cmo core_print.cmo typing.cmo monomorphise.cmo mono.cmo mono_print.cmo translate.cmo test.cmo

all: $(OBJ) hw7.cmo
	$(CAMLC) -o lib.cma -a $(OBJ)
	$(CAMLC) -o $(TARGET) lib.cma hw7.cmo 
	
clean:
	ls $(TARGET) $(PARSER) *.cm[ioxa] | grep -v typing | xargs rm -f 

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

