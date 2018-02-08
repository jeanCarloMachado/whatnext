#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

name=$1
importance=$2
complexity=$3


echo "$name|$importance|$complexity|" >> "$WHATNEXT_CONF"
