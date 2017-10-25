#!/usr/bin/env bash

list_subjects_names() {
    cat ~/.scheduler | cut -d '|' -f1
}

list_subjects() {
    cat ~/.scheduler
}

last_entry_name() {
    tail -n 1 ~/.scheduler_history | cut  -d '|' -f2 | tr -d "\n"
}

last_studied_date_for_subject() {
    tac ~/.scheduler_history | grep "$1" | head -n1 | cut -d '|' -f1 | tr -d "\n"
}

days_since_last_study() {
    date=$(last_studied_date_for_subject "$1")

    # subject never studied
    [ -z "$date" ] && {
        echo 15
        return
    }
    days=$(echo $(( ( $(date -ud "$date" +'%s') - $(date +'%s') )/60/60/24 )))
    days=$(date_diff.js "$date" "$(date)" | jq .days)

    [[ $days ==  'null' ]] && {
        days=0
    }
    echo $days
}

unique_occurence_from_last_to_earlier() {
    cat ~/.scheduler_history | cut  -d '|' -f2 | uniq
}

new_subjects() {
    already_practiced_names=$(unique_occurence_from_last_to_earlier)
    subjects=$(list_subjects_names)

    pattern=$(tr "\n" "|" <<< $already_practiced_names)
    #deletes last pipe
    pattern=${pattern%?}
    [[ -z "$pattern" ]] && {
        echo "$subjects"
        return
    }

    echo "$subjects" | grep -E -v "$pattern"
}


done_today() {
    cat ~/.scheduler_history | grep  "$(date '+%Y-%m-%d')"
}

get_weight_by_name()
{
    list_subjects | grep "$1" | cut -d'|' -f2
}


get_energy_level_by_name()
{
    list_subjects | grep "$1" | cut -d'|' -f3
}

get_whattodo_details_by_name()
{
    list_subjects | grep "$1" | cut -d'|' -f4 | tr -d "\n"
}


done_week() {
    data=$(cat ~/.scheduler_history)
IFS='
'
    first_day_of_week=$(date --date='last sunday' +%Y-%m-%d)
    for entry in $data
    do
        date=$(cut -d ' ' -f1 <<< $entry)
        if [[ $date < $first_day_of_week ]]
        then
            break
        fi

        echo $entry
    done
}

"$@"
