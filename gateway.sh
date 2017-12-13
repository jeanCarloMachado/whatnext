#!/usr/bin/env bash

[[ -z ${WHATNEXT_CONF+x} ]] && {
    WHATNEXT_CONF=~/.whatnext.conf
}

[[ -z ${WHATNEXT_HISTORY+x} ]] && {
    WHATNEXT_HISTORY=~/.whatnext_history
}

listSubjectsNames() {
    cat "$WHATNEXT_CONF" | cut -d '|' -f1
}

listSubjects() {
    cat "$WHATNEXT_CONF"
}

last_entry_name() {
    tail -n 1 "$WHATNEXT_HISTORY" | cut  -d '|' -f2 | tr -d "\n"
}

last_studied_date_for_subject() {
    tac "$WHATNEXT_HISTORY" | grep "$1" | head -n1 | cut -d '|' -f1 | tr -d "\n"
}

daysSinceLastStudy() {
    date=$(last_studied_date_for_subject "$1")

    # subject never studied
    [ -z "$date" ] && {
        return
    }

    start_ts=$(date -d "$date" '+%s')
    end_ts=$(date '+%s')

    echo $(( ( $end_ts - $start_ts )/(60*60*24) )) | tr -d "\n"
}

unique_occurence_from_last_to_earlier() {
    cat "$WHATNEXT_HISTORY" | cut  -d '|' -f2 | uniq
}

new_subjects() {
    already_practiced_names=$(unique_occurence_from_last_to_earlier)
    subjects=$(listSubjectsNames)

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
    cat "$WHATNEXT_HISTORY" | grep  "$(date '+%Y-%m-%d')"
}

get_weight_by_name()
{
    listSubjects | grep "$1" | cut -d'|' -f2
}


get_energy_level_by_name()
{
    listSubjects | grep "$1" | cut -d'|' -f3
}

get_whattodo_details_by_name()
{
    listSubjects | egrep "^$1" | cut -d'|' -f4 | tr -d "\n"
}

done_week() {
    data=$(cat "$WHATNEXT_HISTORY")
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
