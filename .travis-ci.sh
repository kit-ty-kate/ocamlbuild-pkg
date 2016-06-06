# Install OPAM
sudo add-apt-repository --yes ppa:avsm/ppa
sudo apt-get update -qq
sudo apt-get install -qq opam

# Init OPAM
opam init -y $OCAML_VERSION
eval `opam config env`

# Check OPAM package description
opam lint

# Install
opam pin add -y --kind=git ocamlbuild-pkg .

# Run tests
make examples

# Uninstall
opam remove -y ocamlbuild-pkg
