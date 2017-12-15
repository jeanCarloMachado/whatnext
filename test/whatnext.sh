#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

WHATNEXT_BIN="$__dir/../whatnext"
test_description="basic testing"

. sharness.sh

touch /tmp/foo
export WHATNEXT_CONF="/tmp/wncfg"
export WHATNEXT_HISTORY="/tmp/wnhistory"


rm -rf $WHATNEXT_CONF || true
rm -rf $WHATNEXT_HISTORY || true

test_expect_success "expect empty history on empty file" "
    test -z $($WHATNEXT_BIN log)
"

test_expect_success "expect intialized file" "
  $WHATNEXT_BIN init && [[ \"myFirstSubject|50|50|do something\" == \"$(cat /tmp/wn.cfg )\" ]]
"

test_done
