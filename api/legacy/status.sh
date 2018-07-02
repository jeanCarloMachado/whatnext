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

echo -e "Status"
echo -e "Today: $dayColor$doneToday"
echo -e "Yesterday: $dayColor$doneYesterday"
echo -e "Week: $weekColor$doneWeek"
echo -e "Previous Week: $weekColor$donePreviousWeek"
echo -e "Current streak: $weekColor$daysInARow days"

