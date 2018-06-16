#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

export WHATNEXT_SRC=$DIR
export WHATNEXT_CONF=${WHATNEXT_CONF:-~/.whatnext.conf}
export WHATNEXT_GOALS=${WHATNEXT_GOALS:-~/.whatnext_goals.conf}
export WHATNEXT_HISTORY=${WHATNEXT_HISTORY:-~/.whatnext_history}
export WHATNEXT_USERS=${WHATNEXT_USERS:-~/.whatnext_users}
export WHATNEXT_ENVIROMENT=${WHATNEXT_ENVIROMENT:-production}
export WN_COLOR_GREEN="\x1b[32m"
export WN_COLOR_ORANGE="\x1b[33m"
export WN_COLOR_RED="\x1b[31m"
export WN_COLOR_TITLE="\x1b[1;49;93m"
export WN_COLOR_SECTION="\x1b[1;49;95m"
export WN_COLOR_RESET="\x1b[0m"


export EDITOR=${EDITOR:-vim}
