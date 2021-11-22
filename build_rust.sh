#!/usr/bin/env sh

if [ "`uname`" = "Darwin" ]; then
  CAML_LIB="`ocamlc -where`";
  RUSTFLAGS="'-L$CAML_LIB' -lcamlrun" cargo build --release --frozen ;
else
  cargo build --release --frozen ;
fi
