#!/usr/bin/env bash

files=$(ls test/*.sh)

IFS='
'

for i in $files
do
    ./$i
done



