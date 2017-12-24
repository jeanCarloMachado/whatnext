#!/usr/bin/env bash

goalExists() {
    goalName="$1"
    goals=$(cat $WHATNEXT_GOALS)
    result=$(echo "$goals" | jq ".$goalName")
    test "$result" != "null"
}

listSubjectsNames() {
    cat "$WHATNEXT_CONF" | cut -d '|' -f1
}

listSubjects() {
    cat "$WHATNEXT_CONF" | sed -e /^$/d
}

listHistory() {
    cat "$WHATNEXT_HISTORY" | sed -e /^$/d
}

last_entry_name() {
    tail -n 1 "$WHATNEXT_HISTORY" | cut  -d '|' -f2 | tr -d "\n"
}

lastStudiedDateForSubject() {
    subject="$1"
    tac "$WHATNEXT_HISTORY" | grep "|$subject|" | head -n1 | cut -d '|' -f1 | tr -d "\n"
}

daysSinceLastStudy() {
    date=$(lastStudiedDateForSubject "$1")

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


doneToday() {
    cat "$WHATNEXT_HISTORY" | grep  "$(date '+%Y-%m-%d')"
}


missingTimeToTaskByName()
{
    listSubjects | egrep "^$1" | cut -d'|' -f4 | tr -d "\n"
}


doneWeek() {
IFS='
'
    firstDayOfWeek=$(date --date='last sunday' +%s)
    for entry in $(listHistory)
    do
        date=$(cut -d ' ' -f1 <<< $entry)
        realDate=$(date  --date "$date" +%s)
        if [[ $realDate -lt $firstDayOfWeek ]]
        then
            continue
        fi

        echo $entry
    done
}

addWhatToDoNextToSubjet() {
    subject="$1"
    what_to_do_next="$2"
    subject_config=$(cat $WHATNEXT_CONF | egrep "^$subject")
    subject_config_without_description=$(echo "$subject_config" | rev | cut -d '|' -f1 --complement | rev)
    newSubjectConfigEntry="$subject_config_without_description|$what_to_do_next"

    sed -i "/^$subject/d" $WHATNEXT_CONF
    echo "$newSubjectConfigEntry" >> $WHATNEXT_CONF
}

"$@"
