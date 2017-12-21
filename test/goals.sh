#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

WHATNEXT_BIN="$__dir/../whatnext"
test_description="basic testing"

. sharness.sh

touch /tmp/foo
export WHATNEXT_CONF="/tmp/wncfg"
export WHATNEXT_HISTORY="/tmp/wnhistory"
export WHATNEXT_GOALS="/tmp/wngoals"
export NO_COLOR=1


rm -rf $WHATNEXT_CONF || true
rm -rf $WHATNEXT_HISTORY || true
rm -rf $WHATNEXT_GOALS || true

test_expect_success "goal initalization" "
  $WHATNEXT_BIN init &&
  test -f $WHATNEXT_GOALS &&
  echo \"math|90|90|study calculus\" >> $WHATNEXT_CONF
"

today=$(date --date="today" "+%Y-%m-%d")
nextWeek=$(date --date="next Friday" "+%Y-%m-%d")

cat >$WHATNEXT_GOALS <<EOL
{
    "mathToday": {
        "from": "$today",
        "to": "$nextWeek",
        "minutes": 40,
        "subject": "math"
    }
}
EOL

test_expect_success "list goal" "
  $WHATNEXT_BIN goal | grep math &&
  $WHATNEXT_BIN goal | grep -i \"0%\"
"
test_expect_success "mark as done" "
  $WHATNEXT_BIN 'done' math 'first session' &&
  $WHATNEXT_BIN goal | grep -i \"100%\"
"

test_done
