#!/usr/bin/env sh

if [ "`uname`" = "Darwin" ]; then
  CAML_LIB="`ocamlc -where`";
  SEP="`echo '\x1f'`";
  CARGO_ENCODED_RUSTFLAGS="-L$CAML_LIB$SEP-lcamlrun" cargo build --release --frozen ;
else
  cargo build --release --frozen ;
fi
