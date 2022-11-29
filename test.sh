#!/bin/sh -e
ROOT=$PWD

cd ${ROOT}/rs
time cargo run --release
cd ${ROOT}/rs
wasm-pack build --target nodejs
cd ${ROOT}/ts
npx tsc
time node --experimental-wasm-modules index.mjs

