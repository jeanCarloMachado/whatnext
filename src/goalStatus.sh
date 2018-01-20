#!/usr/bin/env bash

# set -o xtrace
__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

goalName="$1"
goals=$($__dir/gateway.sh getGoals)

[[ ! $($__dir/gateway.sh goalExists "$goalName") ]] || {
    echo "No goal named $goalName"
    exit 1
}

subject=$( echo "$goals" | jq ".$goalName"'.subject' -r)
dateStart=$( echo "$goals" | jq ".$goalName"'.from' -r)
dateEnd=$( echo "$goals" | jq ".$goalName"'.to' -r)
minutes=$( echo "$goals" | jq ".$goalName"'.minutes' -r)

doneInPeriod=$($__dir/gateway.sh doneInPeriod "$subject" "$dateStart" "$dateEnd")

timeMissing=$(($minutes - $doneInPeriod))
percentageDone=$(( 100 * $doneInPeriod / $minutes ))
resetColor=$WN_COLOR_RESET
titleColor=$WN_COLOR_TITLE

start_ts=$(date '+%s')
end_ts=$(date -d "$dateEnd" '+%s')
remaingingDays=$(( ( end_ts - start_ts )/(60*60*24) ))

percentageColor=$WN_COLOR_RED
[ $percentageDone -ge 33 ] && {
    percentageColor=$WN_COLOR_ORANGE
}
[ $percentageDone -ge 66 ] && {
    percentageColor=$WN_COLOR_GREEN
}
[ $percentageDone -ge 100 ] && {
    percentageDone=100
}

[ ! -z ${NO_COLOR+x} ] && {
    percentageColor=""
    resetColor=""
    titleColor=""
}

echo -e "$percentageColor$percentageDone$resetColoryour$resetColor% done $titleColor$timeMissing$resetColor min remaining in $remaingingDays days"
