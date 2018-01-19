#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export WHATNEXT_BIN="$__dir/src/whatnext"

files=$(ls test/*.sh)

IFS='
'

for i in $files
do
    ./$i
done



