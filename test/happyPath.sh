#!/usr/bin/env bash

 # set -o xtrace
__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

test_description="basic testing"

. sharness.sh

touch /tmp/foo
export WHATNEXT_CONF="/tmp/wncfg"
export WHATNEXT_HISTORY="/tmp/wnhistory"
export NO_COLOR=1


rm -rf $WHATNEXT_CONF || true
rm -rf $WHATNEXT_HISTORY || true

test_expect_success "expect intialized file" "
  $WHATNEXT_BIN init && grep -i \"myfirst\" $WHATNEXT_CONF
"

test_expect_success "expect empty history after initialization" "
    test -z $($WHATNEXT_BIN log)
"

test_expect_success "setup subject appears on listing" "
      echo \"math|90|90|study calculus\" >> $WHATNEXT_CONF &&
      $WHATNEXT_BIN  | grep math
"

test_expect_success "done subject appears on log" "
     $WHATNEXT_BIN done math \"studied calculus\" &&
     $WHATNEXT_BIN log | grep 'math'
"

test_done
