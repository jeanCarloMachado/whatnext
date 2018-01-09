#!/usr/bin/env bash

__dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

doneToday=$($__dir/gateway.sh donePeriod $(date --date='today 00:00:00' +%s) | wc -l)

doneYesterday=$($__dir/gateway.sh donePeriod $(date --date='yesterday 00:00:00' +%s) $(date --date='yesterday 23:59:59' +%s)  | wc -l)

doneWeek=$($__dir/gateway.sh donePeriod "$(date --date='last sunday' +%s)" | wc -l)


previousWeek=$(python -c "
from datetime import timedelta,datetime 
from dateutil.relativedelta import relativedelta, FR
date = datetime.now() + relativedelta(weekday=FR(-1))
year, week, dow = date.isocalendar()
if dow == 7:
    start_date = date
else:
    start_date = date - timedelta(dow)
end_date = start_date + timedelta(6)
print (start_date.strftime('%s'))
print (end_date.strftime('%s'))
")

previousWeekFrom=$(echo "$previousWeek" | head -n1)
previousWeekTo=$(echo "$previousWeek" | tail -n1)

donePreviousWeek=$($__dir/gateway.sh donePeriod "$previousWeekFrom" "$previousWeekTo" | wc -l)


daysInARow=$($__dir/gateway.sh currentStreak)

resetColor=$WN_COLOR_RESET
sectionColor="\x1b[1;49;95m"
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


[ ! -z ${NO_COLOR+x} ] && {
    resetColor=""
    sectionColor=""
    dayColor=""
    weekColor=""
    WN_COLOR_TITLE=""
}

echo -e "$sectionColor""Status$resetColor"
echo -e "$WN_COLOR_TITLE""Today$resetColor: $dayColor$doneToday$resetColor"
echo -e "$WN_COLOR_TITLE""Yesterday$resetColor: $dayColor$doneYesterday$resetColor"
echo -e "$WN_COLOR_TITLE""Week$resetColor: $weekColor$doneWeek$resetColor"
echo -e "$WN_COLOR_TITLE""Previous Week$resetColor: $weekColor$donePreviousWeek$resetColor"

echo -e "$WN_COLOR_TITLE""Current streak$resetColor: $weekColor$daysInARow$resetColor days"

echo ""
echo -e "$sectionColor""Goals$resetColor"
$__dir/goals.sh | head -n 5

echo ""
echo -e "$sectionColor""Top subjects$resetColor"

$__dir/timePerSubject.py | head -n 5
