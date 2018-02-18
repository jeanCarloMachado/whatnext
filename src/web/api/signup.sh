#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset
# set -o xtrace
scriptDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $scriptDir/../../config.sh

email=$1
password=$2

grep "$email" "$WHATNEXT_USERS" 1>/dev/null && {
    echo -e "User already exists\c"
    exit 1
}


creationDate=$(date "+%Y-%m-%d %H:%M:%S")
echo "$email|$password|$creationDate"  >> "$WHATNEXT_USERS"


