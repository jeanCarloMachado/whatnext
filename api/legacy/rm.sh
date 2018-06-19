#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

subject=$1

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
"$__dir"/gateway.sh subjectExists "$subject" || {
    echo "Subject not found"
    exit 1
}

sed -i "/^$subject|/d" $WHATNEXT_CONF
