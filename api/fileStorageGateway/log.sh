#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IFS='
'

filter=""
if [[ "$*" =~ "--filter"  ]]
then
    shift
    subject="$1"
    log=$( "$__dir"/gateway.sh logEntriesOfSubject "$subject")
else
    log=$( "$__dir"/gateway.sh logEntries)
fi


current_entry=$(echo "$log" | wc -l )

echo "["

for i in $log
do
    date=$(echo $i | cut -d '|' -f1)
    subject=$(echo $i | cut -d '|' -f2)
    description=$(echo $i | cut -d '|' -f3)
    goal=$(echo $i | cut -d '|' -f4)
    duration=$(echo $i | cut -d '|' -f5)



    if [ -z ${skipFirstComma+x} ]
    then
        skipFirstComma=1
    else
        echo -e ",\c"
    fi

echo -e '{
    "subject": "'$subject'",
    "date": "'$date'",
    "description": "'$description'",
    "goal": "'$goal'",
    "duration": '$duration'
}\c'
        continue

    echo -e "task $current_entry: $subject"
    echo "Date: $date"
    [ ! -z "$goal" ] && {
        name=$(echo $goal | cut -d ' ' -f1)
        minutes=$(echo $goal | cut -d ' ' -f2 | tr -d ' ')
        echo -e "Goal: $name completed a total of $minutes minutes"
    }
    [ ! -z "$description" ] && {
        echo "
    $description"
    }
    echo ""
    current_entry=$(( $current_entry - 1 ))

done

echo "]"
