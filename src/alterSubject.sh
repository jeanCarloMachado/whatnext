#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

name=$1
importance=$2
complexity=$3
whatToDoNext=$4
objective=$5

[ -z "$name" ] && {
    echo -e "Subject name cannot be empty\c"
    exit 1
}



sed -r -i "/^$name\|/d"  "$WHATNEXT_CONF"
echo  "$name|$importance|$complexity|$whatToDoNext|$objective" >> "$WHATNEXT_CONF"
