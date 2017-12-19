#!/usr/bin/env bash

# set -o xtrace
__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

timeOnSubject() {
    $__dir/timePerSubject.py --no-color | grep "$1" | cut -d ' ' -f2
}

goalName="$1"
goals=$(cat $WHATNEXT_GOALS)
[[ "$(echo "$goals" | jq ".$goalName" -r | tr -d ' ' )" == 'null' ]] && {
    echo "No goal named $goalName"
    exit 1
}

subject=$( echo "$goals" | jq ".$goalName"'.subject' -r)
dateStart=$( echo "$goals" | jq ".$goalName"'.from' -r)
dateEnd=$( echo "$goals" | jq ".$goalName"'.to' -r)
minutes=$( echo "$goals" | jq ".$goalName"'.minutes' -r)

doneInPeriod=$($__dir/timePerSubject.py "$dateStart" "$dateEnd" --no-color | grep "$subject" | cut -d ' ' -f2 | tr -d " ")
doneInPeriod=${doneInPeriod:-0}

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

[ ! -z ${NO_COLOR+x} ] && {
    percentageColor=""
}

echo -e "You completed $percentageColor $percentageDone$resetColor% of your goal $titleColor$timeMissing$resetColor minutes remaining to do in $remaingingDays days"
