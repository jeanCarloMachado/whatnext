#!/usr/bin/env bash

gethelp() {
    echo "Mark as done a study session

    Options:
        FORCE=1 wn done 'subject that does not exits'
        when you want to mark as done a subject you don't have on your config file
    "
}

[[ "$*" =~ '-h' ]] && {
    gethelp
    exit
}

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

subject="$1"
doneDescription="$2"
nextStep="$3"

doneDescription=$(echo "$doneDescription" | sed ':a;N;$!ba;s/\n/\\n/g')
nextStep=$(echo "$nextStep" | sed ':a;N;$!ba;s/\n/\\n/g')



"$__dir"/gateway.sh subjectExists "$subject"

[ $? -eq 1 ] && [[ -z ${FORCE+x} ]]  && {
    echo 'subject not found'
    exit 1
}


"$__dir"/gateway.sh addWhatToDoNextToSubjet "$subject" "$nextStep"

goalsNamesBefore=$("$__dir"/goals.sh | grep -v "100%" | cut -d ':' -f1)

echo "$( date "+%Y-%m-%d %H:%M:%S")|$subject|$doneDescription|$goalStr" >> $WHATNEXT_HISTORY

goalsToBeDoneAfter=$("$__dir"/goals.sh | grep -v "100%")
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
            echo $( date "+%Y-%m-%d %H:%M:%S")|"$subject"|"$doneDescription"|"$goalStr" >> "$WHATNEXT_HISTORY"
            exit
        }

    done
}

exit 0
