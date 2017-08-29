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
    list_subjects | grep $1 | cut -d'|' -f2
}

get_energy_level_by_name()
{
    list_subjects | grep $1 | cut -d'|' -f3
}

$@
