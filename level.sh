#!/usr/bin/env bash

[[ -z "$@" ]] && {
    echo "
    Display the expertise level in a given subject

        Usage:
        wn level git
    "
    exit
}

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

set -o errexit
set -o pipefail
set -o nounset
# set -o xtrace

subject="$1"
timeSpent=$($__dir/stats.py | grep "$subject" -A1 | tail -n 1 | rev | cut -d ' ' -f1 | rev)
timeSpent=$(($timeSpent / 60))


[[ $timeSpent -lt 50 ]] && {
    echo "Novice"
    exit
}

[[ $timeSpent -lt 200 ]] && {
    echo "Advanced beginner"
    exit
}

[[ $timeSpent -lt 500 ]] && {
    echo "Competent"
    exit
}

[[ $timeSpent -lt 2000 ]] && {
    echo "Proficient"
    exit
}

[[ $timeSpent -gt 9999 ]] && {
    echo "Expert"
    exit
}
