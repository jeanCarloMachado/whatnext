#!/usr/bin/env bash

 # set -o xtrace
__dir="$(cd "$(dirname "${bash_source[0]}")" && pwd)"

whatnext_bin="$__dir/../whatnext"
test_description="basic testing"

. sharness.sh

touch /tmp/foo
export whatnext_conf="/tmp/wncfg"
export whatnext_history="/tmp/wnhistory"
export no_color=1


rm -rf $whatnext_conf || true
rm -rf $whatnext_history || true

$whatnext_bin init &> /dev/null

test_expect_failure "cannot edit empty subject" "
     $whatnext_bin esd
"

test_done
