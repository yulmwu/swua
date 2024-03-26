#!/bin/bash

cargo build

for file in ./examples/*.swua; do
    echo "Executing: $file"
    ./target/debug/swua build -i $file -l -a
    ./build/main
done
