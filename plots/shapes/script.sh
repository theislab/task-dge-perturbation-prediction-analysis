#!/bin/bash

for file in plots/shapes/*.svg; do
  inkscape "$file" -o "${file%.svg}.png"
done