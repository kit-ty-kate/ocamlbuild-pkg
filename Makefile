all:
	cat src/ocamlbuild_pkg.ml build.ml > myocamlbuild.ml
	ocamlbuild

clean:
	rm myocamlbuild.ml
	ocamlbuild -clean

.PHONY: all clean
