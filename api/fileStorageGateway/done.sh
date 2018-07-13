#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

subject="$1"
doneDescription="$2"
nextStep="$3"
duration="$4"

doneDescription=$(echo "$doneDescription" | sed ':a;N;$!ba;s/\n/\\n/g')
nextStep=$(echo "$nextStep" | sed ':a;N;$!ba;s/\n/\\n/g')


"$__dir"/gateway.sh subjectExists "$subject"
[ $? -eq 0 ] && {
		"$__dir"/gateway.sh addWhatToDoNextToSubjet "$subject" "$nextStep"
}


echo "$( date "+%Y-%m-%d %H:%M:%S")|$subject|$doneDescription|$goalStr|$duration" >> $WHATNEXT_HISTORY
