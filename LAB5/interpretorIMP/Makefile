

all: imp

lexer.ml: lexer.mll parser.cmi
	ocamllex lexer.mll

parser.ml: parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.ml
	ocamlc -c parser.mli

types.cmo: types.ml mem.cmo impAST.cmo
	ocamlc -c types.ml

semantics.cmo: semantics.ml mem.cmo impAST.cmo
	ocamlc -c semantics.ml

imp.cmo: imp.ml semantics.cmo types.cmo
	ocamlc -c imp.ml

%.cmo: %.ml
	ocamlc -c $<

imp: impAST.cmo lexer.cmo parser.cmo imp.cmo mem.cmo semantics.cmo types.cmo
	ocamlc -o imp impAST.cmo lexer.cmo parser.cmo mem.cmo semantics.cmo types.cmo imp.cmo

clean:
	rm -f lexer.ml parser.ml parser.mli *.cmo *.cmi imp
