#!/usr/bin/env bash

log=$(tac "$WHATNEXT_HISTORY")
current_entry=$(echo "$log" | wc -l )
IFS='
'
[ ! -z ${NO_COLOR+x} ] && {
    WN_COLOR_ORANGE=""
    WN_COLOR_RESET=""
    WN_COLOR_TITLE=""
    WN_COLOR_GREEN=""
}

for i in $log
do
    date=$(echo $i | cut -d '|' -f1)
    subject=$(echo $i | cut -d '|' -f2)
    description=$(echo $i | cut -d '|' -f3)
    goal=$(echo $i | cut -d '|' -f4)
    echo -e "$WN_COLOR_ORANGE $current_entry $WN_COLOR_TITLE $subject $WN_COLOR_RESET"
    echo " Date:     $date"
    [ ! -z "$goal" ] && {
        name=$(echo $goal | cut -d ' ' -f1)
        minutes=$(echo $goal | cut -d ' ' -f2 | tr -d ' ')
        echo -e "$WN_COLOR_GREEN Goal: $name completed a total of $minutes minutes"
    }
    [ ! -z "$description" ] && {
        echo "      $description"
    }
    echo ""
    current_entry=$(( $current_entry - 1 ))

done

