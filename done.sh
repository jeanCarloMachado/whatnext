#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

subject="$1"

[[ ! $($__dir/gateway.sh listSubjectsNames | grep "$subject") ]]  && {
    echo 'subject not found'
    exit 1
}

#process what to do next
[[ ! -z ${3+x} ]] && {
    what_to_do_next="$4"
    subject_config=$(cat $WHATNEXT_HISTORY | grep "$subject")
    subject_config_without_description=$(echo "$subject_config" | rev | cut -d '|' -f1 --complement | rev)
    newSubjectConfigEntry="$subject_config_without_description|$what_to_do_next"

    sed -i "/^$subject/d" $WHATNEXT_HISTORY
    echo "$newSubjectConfigEntry" >> $WHATNEXT_HISTORY
}


echo "$( date "+%Y-%m-%d %H:%M:%S")|$subject|$2" >> $WHATNEXT_HISTORY
