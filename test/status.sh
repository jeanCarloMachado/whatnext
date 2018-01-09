#!/usr/bin/env bash

 # set -o xtrace
scriptDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

whatnextBin="$scriptDir/../whatnext"
statusBin="$scriptDir/../status.sh"
test_description="status"

. sharness.sh

touch /tmp/foo
export WHATNEXT_CONF="/tmp/wncfg"
export WHATNEXT_HISTORY="/tmp/wnhistory"
export WHATNEXT_GOALS="/tmp/wngoals"
export NO_COLOR=1

rm -rf $WHATNEXT_CONF || true
rm -rf $WHATNEXT_HISTORY || true

$whatnextBin init &> /dev/null

test_expect_success "test has fields" "
     $statusBin | grep -i today
"

test_done
