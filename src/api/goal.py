#!/usr/bin/env python3

from gateway import gateway, gatewaySuccess
import json
import sys
from datetime import datetime,date

def diff_dates(date1, date2):
    return (date2-date1).days

goal = sys.argv[1]

if not gatewaySuccess(['goalExists', goal]):
    print("The given goal don't exists")
    sys.exit()

goals = gateway(['getGoals'])
goals_json = json.loads(goals)

subject = goals_json[goal]['subject']
date_start = goals_json[goal]['from']
date_end = goals_json[goal]['to']
minutes = goals_json[goal]['minutes']

done_in_period = int(gateway(['doneInPeriod', subject, date_start, date_end]))


time_missing = minutes - done_in_period
now = datetime.now()
date_end_datetime = datetime.strptime(date_end, "%Y-%m-%d")

remaining_days = diff_dates(now, date_end_datetime)
percentage_done = 100 * done_in_period / minutes 

if percentage_done > 100:
    percentage_done = 100

print(str(int(percentage_done)) + "% done " + str(time_missing) + " min remaining in " + str (remaining_days) + " days")
