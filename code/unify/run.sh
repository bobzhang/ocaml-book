#!/usr/bin/env bash 
ocamlbuild -clean
# ocamlbuild mlast.pp.ml
# cat _build/mlast.pp.ml
ocamlbuild pa_ml.cma
# ocamlbuild test_pa_ml.pp.ml
# cat _build/test_pa_ml.pp.ml
ocamlbuild ml_infer.cma