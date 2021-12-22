#!/usr/bin/env sh

if [ "`uname`" = "Darwin" ]; then
  CAML_LIB="`ocamlc -where`";
  CARGO_ENCODED_RUSTFLAGS="$(printf -- "-L%s\x1f-lcamlrun" "$CAML_LIB")" cargo build --release --frozen ;
else
  cargo build --release --frozen ;
fi
