#!/bin/bash

echo "Converts all .txt files to .png"

for filename in *.txt; do
    text2png.py $filename ../img/$filename.png -f FreeMono.ttf -s 13 -p 12 -f DejaVuSansMono-Bold.ttf
done