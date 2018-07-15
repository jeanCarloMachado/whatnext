#!/usr/bin/env bash

echo $WHATNEXT_CONF
mkdir -p $(dirname "$WHATNEXT_CONF")

[ ! -f "$WHATNEXT_CONF" ] && {
   touch "$WHATNEXT_CONF"
}

[ ! -f "$WHATNEXT_HISTORY" ] && {
   touch "$WHATNEXT_HISTORY"

}

exit 0
