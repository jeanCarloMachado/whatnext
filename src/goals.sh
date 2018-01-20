#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
goals=$(cat $WHATNEXT_GOALS | jq 'keys[]' -r)

IFS='
'

[ ! -z ${NO_COLOR+x} ] && {
    WN_COLOR_TITLE=""
    WN_COLOR_RESET=""
}

data=""
for i in $goals
do
    data="$(echo -e "$WN_COLOR_TITLE$i:$WN_COLOR_RESET "$($__dir/goal.py $i ))
$data"
done

echo "$data" | egrep -v "^$" | sort -k2 -t " "  -r
