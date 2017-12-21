#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

subject="$1"
doneDescription="$2"

[ -z "$subject" ] || [[ ! $($__dir/gateway.sh listSubjectsNames | grep "$subject") ]]  && {
    echo 'subject not found'
    exit 1
}

goalsNamesBefore=$(NO_COLOR=1 $__dir/goals.sh | grep -v "100%" | cut -d ':' -f1)

echo "$( date "+%Y-%m-%d %H:%M:%S")|$subject|$doneDescription|$goalStr" >> $WHATNEXT_HISTORY

goalsToBeDoneAfter=$(NO_COLOR=1 $__dir/goals.sh | grep -v "100%")
[ $(echo "$goalsNamesBefore" | wc -l) -ne $( echo "$goalsToBeDoneAfter" | wc -l ) ] && {
    goals=$(cat $WHATNEXT_GOALS)

IFS='
'
    for goalName in $goalsNamesBefore
    do
        #if the goal is not anymore between the one's to be done it's  done
        echo "$goalsToBeDoneAfter" | grep "$goalName"  &> /dev/null
        [[ $? -ne 0 ]] && {
            minutes=$( echo "$goals" | jq ".$goalName"'.minutes' -r)
            #remove the previously added entry to readd with the goal info
            sed -i '$ d' $WHATNEXT_HISTORY
            goalStr="$goalName $minutes"
            echo "$( date "+%Y-%m-%d %H:%M:%S")|$subject|$doneDescription|$goalStr" >> $WHATNEXT_HISTORY
            exit
        }

    done
}

exit 0
