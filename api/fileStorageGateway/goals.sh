#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
goals=$(cat $WHATNEXT_GOALS | jq 'keys[]' -r)

IFS='
'

data=""
for i in $goals
do
    data="$(echo -e "$i: "$($__dir/goal.py $i ))
$data"
done

echo "$data" | egrep -v "^$" | sort -k2 -t " "  -r
