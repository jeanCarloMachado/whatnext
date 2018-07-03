#!/usr/bin/env bash

subjectToJson() {
    subjectRow="$1"

    sanitizedSubjectRow=$(echo "$subjectRow" | sed 's/\\n/\\n/g')
    name=$(cut -d "|" -f1 <<< $sanitizedSubjectRow)
    priority=$(cut -d "|" -f2 <<< $sanitizedSubjectRow)
    complexity=$(cut -d "|" -f3 <<< $sanitizedSubjectRow)
    whatToDoNext=$(echo "$sanitizedSubjectRow" | cut -d "|" -f4 )
    objective=$(echo "$sanitizedSubjectRow" | cut -d "|" -f5 )
    creationDate=$(echo "$sanitizedSubjectRow" | cut -d "|" -f6 )


    echo "{"
    echo '  "name": "'$name'",'
    echo '  "priority": '$priority','
    echo '  "complexity": '$complexity','
    echo '  "whatToDoNext": "'$whatToDoNext'",'
    echo '  "objective": "'$objective'",'
    echo '  "creationDate": "'$creationDate'"'
    echo "}"
}


[ ! -z "$1" ] && {
    row=$(grep  "$1|" "$WHATNEXT_CONF")
    subjectToJson "$row"
    exit 0
}


IFS='
'

echo "["
for i in $(egrep -v "^$" "$WHATNEXT_CONF")
do


    if [ -z ${skipFirstComma+x} ]
    then
        skipFirstComma=1
    else
        echo ","
    fi

    subjectToJson "$i"


done

echo "]"
