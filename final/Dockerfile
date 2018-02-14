FROM ocaml/opam:ubuntu-16.10_ocaml-4.04.0

RUN opam update && opam upgrade && opam install angstrom oasis

WORKDIR  /home/opam

RUN git clone git@github.com:sribst/prog-compare.git

WORKDIR /home/opam/prog-compare/final/
CMD oasis setup && make && ./test.sh
