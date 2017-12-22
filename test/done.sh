#!/usr/bin/env bash

 # set -o xtrace
__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

WHATNEXT_BIN="$__dir/../whatnext"
test_description="basic testing"

. sharness.sh

touch /tmp/foo
export WHATNEXT_CONF="/tmp/wncfg"
export WHATNEXT_HISTORY="/tmp/wnhistory"
export NO_COLOR=1


rm -rf $WHATNEXT_CONF || true
rm -rf $WHATNEXT_HISTORY || true

$WHATNEXT_BIN init

test_expect_failure "cannot done empty subject" "
     $WHATNEXT_BIN done
"

test_done
