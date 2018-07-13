#!/usr/bin/env bash

echo $WHATNEXT_CONF
mkdir -p $(dirname "$WHATNEXT_CONF")

[ ! -f "$WHATNEXT_CONF" ] && {
   creationDate=$(date "+%Y-%m-%d")
   echo "my first subject|50|50|do something||$creationDate" > "$WHATNEXT_CONF"
}

[ ! -f "$WHATNEXT_HISTORY" ] && {
   touch "$WHATNEXT_HISTORY"
   signupDate=$(date "+%Y-%m-%d %H:%M:%S")
   echo "$signupDate|studies|started using whatnext!|" > "$WHATNEXT_HISTORY"

}

exit 0
