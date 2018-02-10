#!/usr/bin/env bash

echo $WHATNEXT_CONF
mkdir -p $(dirname "$WHATNEXT_CONF")

[ ! -f "$WHATNEXT_CONF" ] && {
   echo "myFirstSubject|50|50|do something" > "$WHATNEXT_CONF"
}

[ ! -f "$WHATNEXT_HISTORY" ] && {
   touch "$WHATNEXT_HISTORY"
}

[ ! -f "$WHATNEXT_GOALS" ] && {
   touch "$WHATNEXT_GOALS"
}


exit 0
