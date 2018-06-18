#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
subject=$1
[[ -z "$subject"  ]] || [[ ! $($__dir/gateway.sh listSubjectsNames | egrep "^$subject") ]]  && {
    echo 'subject not found'
    exit 1
}
file=$(echo /tmp/$(date "+%Y-%m-%d_%H-%I-%S"))
subjectDescription=$($__dir/gateway.sh listSubjects | egrep "^$subject" | rev | cut -d'|' -f1 | rev )
echo "$subjectDescription" | tr '_' '\n' > $file

$EDITOR "$file"
newContent=$(cat $file | tr '\n' '_')
$__dir/gateway.sh addWhatToDoNextToSubjet "$subject" "$newContent"
exit
