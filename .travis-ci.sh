case "$OCAML_VERSION" in
    4.01) ppa=avsm/ocaml41+opam12;;
    4.02) ppa=avsm/ocaml42+opam12;;
    *) echo Unknown $OCAML_VERSION; exit 1;;
esac

sudo add-apt-repository --yes ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
export OPAMYES=1
opam init
eval `opam config env`
opam pin add -n --kind=git ocamlbuild-pkg .
opam install ocamlbuild-pkg
