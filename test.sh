#!/bin/sh -e

cd rs
wasm-pack build --target nodejs
cd ..
cd ts
npx tsc
node --experimental-wasm-modules index.mjs