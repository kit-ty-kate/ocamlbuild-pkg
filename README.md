An ocamlbuild plugin that helps packaging softwares.

The git repository is located at: https://github.com/jpdeplaix/ocamlbuild-pkg

[![Build Status](https://travis-ci.org/jpdeplaix/ocamlbuild-pkg.png?branch=master)](https://travis-ci.org/jpdeplaix/ocamlbuild-pkg)

### Goal

The current goal of this ocamlbuild plugin is to provide rules that generate
useful files for packaging and build such as .mllib files, substitutions files
(file.in -> file), library file (META) or opam file (.install).
You can generate them separatly, using the correct module, or everything
togather useful for standard programs/libs.
NOTE: The plugin doesn't touch to the build-system (\_tags and so on)
and automatically add targets listed in the myocamlbuild.ml.

### Requirements

* The latest OPAM (https://opam.ocaml.org/)
* To know other requirements, see the `opam` file.

### Installation

From the sources:
```
$ opam pin add ocamlbuild-pkg .
```

Using the latest available release:
```
$ opam install ocamlbuild-pkg
```

### Usage

Create a myocamlbuild.ml using the examples (see the `examples` directory) and
call ocamlbuild with the following arguments:
```
-use-ocamlfind -plugin-tag "package(ocamlbuild-pkg)"
```


Enjoy !
