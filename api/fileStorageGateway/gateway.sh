#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"


getChildren() {
    parent="$1"
    grep -r "|$parent$" $WHATNEXT_CONF | cut -d'|' -f1
}

getEmailByHash() {
    loginHash=$1

    grep "$loginHash" $WHATNEXT_USERS | cut -d '|' -f 1 | tr -d  "\n"
}

validLogin() {
    email=$1
    loginHash=$2

    grep "$email|$loginHash" $WHATNEXT_USERS 1>/dev/null
}

logEntries() {
    tac "$WHATNEXT_HISTORY"
}

logEntriesOfSubject() {
    subject="$1"
    logEntries | grep "$subject"
}

listSubjects() {
    egrep -v "^$" "$WHATNEXT_CONF"
}

listSubjectsNames() {
    listSubjects | cut -d '|' -f1
}

listSubject() {
    subject=$1
    cat "$WHATNEXT_CONF" | grep "$subject|" | sed -e /^$/d
}

subjectJson() {
    subject=$1
    $__dir/conf2json.sh "$subject"
}


subjectExists() {
    subject="$1"
    [[ -z "$subject" ]] && {
        return 1
    }

    listSubjectsNames | egrep "^$subject$" 1>/dev/null
}


listHistoryDesc() {
    tac "$WHATNEXT_HISTORY" | sed -e /^$/d
}

listHistory() {
    cat "$WHATNEXT_HISTORY" | sed -e /^$/d
}

lastEntryName() {
    tail -n 1 "$WHATNEXT_HISTORY" | cut  -d '|' -f2 | tr -d "\n"
}

lastStudiedDateForSubject() {
    subject="$1"
    tac "$WHATNEXT_HISTORY" | grep "|$subject|" | head -n1 | cut -d '|' -f1 | tr -d "\n"
}

doneInPeriod() {
    subject="$1"
    dateStart="$2"
    dateEnd="$3"

    doneInPeriod=$($__dir/timePerSubject.py "$dateStart" "$dateEnd" |
        grep "$subject" | cut -d ':' -f2 | tr -d " ")
    doneInPeriod=${doneInPeriod:-0}
    echo $doneInPeriod
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


donePeriod() {
from=$1
to="$2"
[[ -z "$to" ]] && {
    to=$(date -d'today 23:59:59' +%s)
}
IFS='
'
    for entry in $(listHistory)
    do
        date=$(cut -d ' ' -f1 <<< $entry)
        realDate=$(date  --date "$date 00:00:00" +%s)
        if [[ $realDate -lt $from ]]
        then
            continue
        fi
        if [[ $realDate -gt $to ]]
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
    subject_config_without_objective=$(echo "$subject_config" | rev | cut -d '|' -f1 --complement | rev)
    subject_config_without_next=$(echo "$subject_config_without_objective" | rev | cut -d '|' -f1 --complement | rev)
    subject_config_objective=$(echo "$subject_config" | rev | cut -d '|' -f1 | rev )
    newSubjectConfigEntry="$subject_config_without_next|$what_to_do_next|$subject_config_objective"

    sed -i "/^$subject/d" $WHATNEXT_CONF
    echo "$newSubjectConfigEntry" >> $WHATNEXT_CONF
}

currentStreak() {
    hist=$(listHistoryDesc)

IFS='
'
    streak=0
    daysAgoCounter=1

    while true
    do
        refDate=$(date --date "$daysAgoCounter day ago" "+%Y-%m-%d")
        (echo "$hist" | grep "$refDate" &>/dev/null) || break
        daysAgoCounter=$(($daysAgoCounter + 1))
        streak=$(($streak + 1))
    done

    echo $streak
}

"$@"
