#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

subject="$2"

[[ ! $($__dir/gateway.sh list_subjects_names | grep "$subject") ]]  && {
    echo 'subject not found'
    exit 1
}

#process what to do next
[[ ! -z ${4+x} ]] && {
    what_to_do_next="$4"
    subject_config=$(cat ~/.whatnext.conf | grep "$subject")
    subject_config_without_description=$(echo "$subject_config" | rev | cut -d '|' -f1 --complement | rev)
    newSubjectConfigEntry="$subject_config_without_description|$what_to_do_next"

    sed -i "/^$subject/d" ~/.whatnext.conf
    echo "$newSubjectConfigEntry" >> ~/.whatnext.conf
}


echo "$( date "+%Y-%m-%d %H:%M:%S")|$subject|$3" >> ~/.whatnext_history
