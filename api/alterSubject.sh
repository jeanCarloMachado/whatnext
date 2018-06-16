#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

name="$1"
importance="$2"
complexity="$3"
whatToDoNext="$4"
objective="$5"
creation=$(date "+%Y-%m-%d")


objective=$(echo "$objective" | sed ':a;N;$!ba;s/\n/\\n/g')
whatToDoNext=$(echo "$whatToDoNext" | sed ':a;N;$!ba;s/\n/\\n/g')


[ -z "$name" ] && {
    echo -e "Subject name cannot be empty\c"
    exit 1
}

sed -r -i "/^$name\|/d"  "$WHATNEXT_CONF"
echo  "$name|$importance|$complexity|$whatToDoNext|$objective|$creation" >> "$WHATNEXT_CONF"
