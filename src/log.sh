#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IFS='
'
[ ! -z ${NO_COLOR+x} ] && {
    WN_COLOR_ORANGE=""
    WN_COLOR_RESET=""
    WN_COLOR_TITLE=""
    WN_COLOR_GREEN=""
}

[[ "$*" =~ "--help"  ]]  && {

    echo "View the log

        Options:
            --filter <filterStr>    to filter by text
    "
    exit 0
}


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

[[ $* =~ "--json" ]] && {
    echo "["
}

for i in $log
do
    date=$(echo $i | cut -d '|' -f1)
    subject=$(echo $i | cut -d '|' -f2)
    description=$(echo $i | cut -d '|' -f3)
    goal=$(echo $i | cut -d '|' -f4)


    [[ $* =~ "--json" ]] && {

    if [ -z ${skipFirstComma+x} ]
    then
        skipFirstComma=1
    else
        echo ","
    fi

echo '{
    "subject": "'$subject'",
    "date": "'$date'",
    "description": "'$description'",
    "goal": "'$goal'"
}'
        continue
    }

    echo -e $WN_COLOR_ORANGE"task $current_entry: $subject$WN_COLOR_RESET"
    echo "Date: $date"
    [ ! -z "$goal" ] && {
        name=$(echo $goal | cut -d ' ' -f1)
        minutes=$(echo $goal | cut -d ' ' -f2 | tr -d ' ')
        echo -e "$WN_COLOR_GREEN Goal: $name completed a total of $minutes minutes"
    }
    [ ! -z "$description" ] && {
        echo "
    $description"
    }
    echo ""
    current_entry=$(( $current_entry - 1 ))

done

[[ $* =~ "--json" ]] && {
    echo "]"
}
