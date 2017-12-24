#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
echo "Done Today: "$($__dir/gateway.sh doneToday | wc -l)
echo "Done Week: "$($__dir/gateway.sh doneWeek | wc -l)
