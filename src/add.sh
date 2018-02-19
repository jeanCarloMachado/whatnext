#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

name=$1
importance=$2
complexity=$3
whatToDoNext=$4

[ -z "$name" ] && {
    echo -e "Subject name cannot be empty\c"
    exit 1
}


__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "$name|$importance|$complexity|$whatToDoNext" >> "$WHATNEXT_CONF"
