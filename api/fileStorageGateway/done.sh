#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

subject="$1"
doneDescription="$2"
duration="$3"

doneDescription=$(echo "$doneDescription" | sed ':a;N;$!ba;s/\n/\\n/g')
nextStep=$(echo "$nextStep" | sed ':a;N;$!ba;s/\n/\\n/g')


echo "$( date "+%Y-%m-%d %H:%M:%S")|$subject|$doneDescription|$goalStr|$duration" >> $WHATNEXT_HISTORY
