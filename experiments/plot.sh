#!/bin/bash

if [[ $# -ne 1 ]]; then
  echo "usage:" $0 "<experiment-name>"
  exit 1
fi

mkdir -p data/$1/png
mv data/$1/png/*.png data-backup/$1/png 2> /dev/null

shopt -s nullglob
for file in data/$1/csv/*.csv; do
  name=$(basename $file .csv)
  octave --no-gui smyth_plot.m $1 $name
done
