all: unit.native

test: unit.native
	./unit.native -verbose

unit.native:
	ocamlbuild -use-ocamlfind test/unit.native

.PHONY: unit.native test clean

clean:
	rm -f *.native
	rm -Rf _build

