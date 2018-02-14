
ocamlopt -o gen generateBigMama.ml
./gen $1 $2 $3 $4 $5 $6 $7
rm *.cmi *.cmx *.o gen
