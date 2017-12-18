#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

WHATNEXT_BIN="$__dir/../whatnext"
test_description="basic testing"

. sharness.sh

touch /tmp/foo
export WHATNEXT_CONF="/tmp/wncfg"
export WHATNEXT_HISTORY="/tmp/wnhistory"
export WHATNEXT_GOALS="/tmp/wngoals"


rm -rf $WHATNEXT_CONF || true
rm -rf $WHATNEXT_HISTORY || true

test_expect_success "goal initalization" "
  $WHATNEXT_BIN init &&
  test -f $WHATNEXT_GOALS &&
  echo \"math|90|90|study calculus\" >> $WHATNEXT_CONF
"

echo '
{
    "mathToday": {
        "from": "2017-12-18",
        "to": "2017-12-20",
        "minutes": 40,
        "subject": "math"
    }
}
' > $WHATNEXT_GOAL


test_expect_success "list goal" "
  $WHATNEXT_BIN goal | grep math &&
  $WHATNEXT_BIN goal | grep 0
"


test_expect_success "mark as done" "
  $WHATNEXT_BIN done math 'first session' &&
  $WHATNEXT_BIN goal | grep 125
"

test_done
