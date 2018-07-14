#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

name="$1"
importance="$2"
complexity="$3"
whatToDoNext="$4"
objective="$5"
previousName="$6"
parent="$7"
creation=$(date "+%Y-%m-%d")


[ -z "$previousName" ] && {
  previousName=$name
}

objective=$(echo "$objective" | sed ':a;N;$!ba;s/\n/\\n/g')
whatToDoNext=$(echo "$whatToDoNext" | sed ':a;N;$!ba;s/\n/\\n/g')


[ -z "$name" ] && {
    echo -e "Subject name cannot be empty\c"
    exit 1
}

sed -r -i "/^$previousName\|/d"  "$WHATNEXT_CONF"
echo  "$name|$importance|$complexity|$whatToDoNext|$objective|$creation|$parent" >> "$WHATNEXT_CONF"
