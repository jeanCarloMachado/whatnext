#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

doneToday=$($__dir/gateway.sh donePeriod $(date --date='today 00:00:00' +%s) | wc -l)

doneYesterday=$($__dir/gateway.sh donePeriod $(date --date='yesterday 00:00:00' +%s) $(date --date='yesterday 23:59:59' +%s)  | wc -l)

doneWeek=$($__dir/gateway.sh donePeriod $(date --date='last sunday' +%s) | wc -l)

previousWeekFrom=$(date --date='sunday-fortnight ago' +%s)
previousWeekTo=$(date --date='saturday-fortnight ago 23:59:59' +%s)
donePreviousWeek=$($__dir/gateway.sh donePeriod $previousWeekFrom $previousWeekTo | wc -l)

resetColor=$WN_COLOR_RESET
[ $doneToday -ge $doneYesterday ] && {
    dayColor=$WN_COLOR_GREEN
} || {
    dayColor=$WN_COLOR_RED
}
[ $doneWeek -ge $donePreviousWeek ] && {
    weekColor=$WN_COLOR_GREEN
} || {
    weekColor=$WN_COLOR_RED
}


echo -e "\x1b[1;49;95m""Status$resetColor"
echo -e "$WN_COLOR_TITLE""Today$resetColor: $dayColor$doneToday$resetColor"
echo -e "$WN_COLOR_TITLE""Yesterday$resetColor: $dayColor$doneYesterday$resetColor"
echo -e "$WN_COLOR_TITLE""Week$resetColor: $weekColor$doneWeek$resetColor"
echo -e "$WN_COLOR_TITLE""Previous Week$resetColor: $weekColor$donePreviousWeek$resetColor"


echo ""
echo -e "\x1b[1;49;95m""Goals$resetColor"
./goals.sh | head -n 5

echo ""
echo -e "\x1b[1;49;95m""Top subjects$resetColor"

./timePerSubject.py | head -n 5
