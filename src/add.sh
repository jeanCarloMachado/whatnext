#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

name=$1
importance=$2
complexity=$3


__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "$name|$importance|$complexity|" >> "$WHATNEXT_CONF"
