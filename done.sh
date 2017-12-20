#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

subject="$1"

[[ ! $($__dir/gateway.sh listSubjectsNames | grep "$subject") ]]  && {
    echo 'subject not found'
    exit 1
}

echo "$( date "+%Y-%m-%d %H:%M:%S")|$subject|$2" >> $WHATNEXT_HISTORY
