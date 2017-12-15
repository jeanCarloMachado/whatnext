#!/usr/bin/env python

import operator
import subprocess
import datetime
import sys
import math
from datetime import datetime, timedelta
import os

if len(sys.argv) > 1 and any(map(lambda x: x == "help", sys.argv)):
    print("""Prints the total of time one spent on each subject
Usage:

    program month               returns the totals of the month
    program week                returns the totals of the year

            """)
    sys.exit()

def gateway(params):
    prefix = ['gateway.sh']
    return subprocess.run(prefix + params, stdout=subprocess.PIPE).stdout.decode('UTF-8')

dateStart = datetime.strptime('1970-01-01', '%Y-%m-%d')
dateEnd = datetime.strptime('3000-01-01', '%Y-%m-%d')

if len(sys.argv) > 1 and any(map(lambda x: x == "month", sys.argv)):
    dateStart = datetime.now().replace(day=1)
    dateEnd = dateStart + timedelta(days=30) 
if len(sys.argv) > 1 and any(map(lambda x: x == "week", sys.argv)):
    today = datetime.now()
    dateStart = today - timedelta(days=today.weekday())
    dateEnd = dateStart + timedelta(days=6)

if len(list(filter(lambda x: x != "--no-color", sys.argv))) == 3:
    dateStart = datetime.strptime(sys.argv[1], '%Y-%m-%d')
    dateEnd = datetime.strptime(sys.argv[2], '%Y-%m-%d')

reset = os.getenv('WN_COLOR_RESET').encode('utf-8').decode('unicode_escape')
title = os.getenv('WN_COLOR_TITLE').encode('utf-8').decode('unicode_escape')
orange = os.getenv('WN_COLOR_ORANGE').encode('utf-8').decode('unicode_escape')

if len(sys.argv) > 1 and any(map(lambda x: x == "--no-color", sys.argv)):
    title=''
    reset=''

history = gateway(['listHistory'])
subjectsConfigs = {}
subjects = gateway(['listHistory'])
#build the initial dic
subjectData = {}

for line in subjects.splitlines():
    columns  = line.split('|')
    name=columns[1]
    doneStartStr=columns[0]
    doneStart = datetime.strptime(doneStartStr, '%Y-%m-%d %H:%M:%S')

    if doneStart > dateStart and doneStart < dateEnd:
        subjectData[name] = subjectData[name]+50 if name in subjectData else 50;

sortedSubjects = sorted(subjectData.items(), key=lambda value: value[1], reverse=True)

for item in sortedSubjects:
    print( title + item[0] + reset + ": " + orange + str(item[1]) + reset)

