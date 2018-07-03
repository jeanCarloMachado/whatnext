#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset
# set -o xtrace
scriptDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cat $WHATNEXT_USERS | cut -d '|' -f1

