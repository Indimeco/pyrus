#!/bin/bash

echo "=> Compiling"
elm make ./src/Main.elm --output main.js
echo "=> Done"
