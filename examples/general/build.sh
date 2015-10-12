#!/usr/bin/env bash

echo "===BUILDING==="
rustc --crate-type=dylib -L ../../target/debug general.rs
clang -L "$PWD" -l general  general.c  -o general

echo "===RUNNING==="
LD_LIBRARY_PATH="$PWD:$LD_LIBRARY_PATH" ./general

echo "===CLEANING==="
rm general general.h libgeneral.dylib
