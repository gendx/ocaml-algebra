case "$OCAML_VERSION" in
3.12.1) ppa=avsm/ocaml312+opam10 ;;
4.01.0) ppa=avsm/ocaml41+opam10 ;;
*) echo Unknown $OCAML_VERSION; exit 1 ;;
esac

sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml

ocamlbuild src/main.native

