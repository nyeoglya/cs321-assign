#!/bin/bash

file_path="hw2.cmo"

echo "reading: $file_path"

make
ocamlc -o eval.out $file_path eval.ml

./eval.out

