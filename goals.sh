#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
goals=$(cat $WHATNEXT_GOALS | jq 'keys[]' -r)

IFS='
'

for i in $goals
do
    echo -e "$WN_COLOR_TITLE $i $WN_COLOR_RESET"
    echo  "    $($__dir/goalStatus.sh $i )"

done

