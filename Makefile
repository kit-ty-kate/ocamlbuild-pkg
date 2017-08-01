EXAMPLE_DIRS = \
        full \

BOOTSTRAP_DEPS = \
        src/ocamlbuild_pkg_LazyMonad \
        src/ocamlbuild_pkg_Options \
        src/ocamlbuild_pkg_List \
        src/ocamlbuild_pkg \

all:
	ocaml setup-bootstrap.ml $(BOOTSTRAP_DEPS) > myocamlbuild.ml
	ocamlbuild -use-ocamlfind ocamlbuild-pkg

clean: examples-clean
	rm -f myocamlbuild.ml
	ocamlbuild -clean

examples:
	$(foreach d, $(EXAMPLE_DIRS), $(MAKE) -C examples/$(d);)

examples-clean:
	$(foreach d, $(EXAMPLE_DIRS), $(MAKE) -C examples/$(d) clean;)

check:
	dead_code_analyzer.opt --all -S -bind-seq _build/src

.PHONY: all clean examples examples-clean check
