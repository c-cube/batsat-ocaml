#!/bin/sh

exec dune exec src/diamond_solver/diamond_solver.exe --profile=release -- $@
