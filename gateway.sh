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

unique_occurence_from_last_to_earlier() {
    cat ~/.scheduler_history | cut  -d '|' -f2 | uniq
}

new_subjects() {

    alread_practiced_names=$(unique_occurence_from_last_to_earlier)
    subjects=$(list_subjects_names)

    pattern=$(tr "\n" "|" <<< $already_practiced_names)
    #deletes last pipe
    pattern=${pattern%?}
    [ -z $pattern ] && {
        echo "$subjects"
        return
    }

    echo "$subjects" | grep -E -v "$pattern"
}



[[ "$1"  == "-d" ]] && {
    echo "$( date "+%Y-%m-%d %H:%M:%S")|$2|$3" >> ~/.scheduler_history
    exit
}

[[ "$1"  == "--done-today" ]] && {
    done_today
    exit
}


[[ "$1"  == "--list-subjects" ]] && {
    $__dir/gateway.sh list_subjects_names
    exit
}

[[ "$1"  == "--edit-subjects" ]] && {
    vim ~/.scheduler
    exit
}


$@
