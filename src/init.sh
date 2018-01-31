#!/usr/bin/env bash

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
