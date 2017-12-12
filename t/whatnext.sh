#!/usr/bin/env bash

test_description="basic testing"

. sharness.sh

touch /tmp/foo

test_expect_success "expect empty history on empty file" "
test -z $(WHATNEXT_HISTORY=/tmp/foo ../whatnext log)
"


test_done
