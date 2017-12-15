#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset
# set -o xtrace
__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

timeOnSubject() {
    $__dir/timePerSubject.py --no-color | grep "$1" | cut -d ' ' -f2
}

subject="$1"
goals=$(cat ~/.whatnext_goals.conf)
[[ "$(echo "$goals" | jq '.["'"$subject"'"]' -r | tr -d ' ' )" == 'null' ]] && {
    echo "No goal for the given subject $subject"
    exit 1
}

dateStart=$( echo "$goals" | jq '.["'"$subject"'"][0].from' -r)
dateEnd=$( echo "$goals" | jq '.["'"$subject"'"][0].to' -r)
minutes=$( echo "$goals" | jq '.["'"$subject"'"][0].minutes' -r)

echo $dateStart
echo $dateEnd
echo $minutes



