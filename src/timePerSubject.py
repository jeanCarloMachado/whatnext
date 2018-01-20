#!/usr/bin/env python3

import operator
import datetime
import sys
import math
from datetime import datetime, timedelta
import os
from timeToStr import minutes_to_str
from gateway import gateway

humanMode=False

def time_of_subjects(dateStart=None, dateEnd=None):
    if dateStart is None:
        dateStart = datetime.strptime('1970-01-01', '%Y-%m-%d')
    if dateEnd is None:
        dateEnd = datetime.strptime('3000-01-01', '%Y-%m-%d')

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

    return subjectData


if __name__ == '__main__':
    if len(sys.argv) > 1 and any(map(lambda x: x == "help", sys.argv)):
        print("""The total of time one spent on each subject
    Usage:

        program month               returns the totals of the month
        program week                returns the totals of the year

                """)
        sys.exit()


    if len(sys.argv) > 1 and any(map(lambda x: x == "human", sys.argv)):
        humanMode=True

    dateStart = None
    dateEnd = None
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

    subjectData=time_of_subjects(dateStart, dateEnd)

    sortedSubjects = sorted(subjectData.items(), key=lambda value: value[1], reverse=True)

    resetColor = os.getenv('WN_COLOR_RESET').encode('utf-8').decode('unicode_escape')
    titleColor = os.getenv('WN_COLOR_TITLE').encode('utf-8').decode('unicode_escape')
    orange = os.getenv('WN_COLOR_ORANGE').encode('utf-8').decode('unicode_escape')


    if os.environ.get('NO_COLOR') is not None:
        titleColor=''
        resetColor=''
        orange=''

    total = 0
    for item in sortedSubjects:
        total+= item[1]
        timeInSubject = minutes_to_str(item[1]) if (humanMode) else  str(item[1])
        print( titleColor + item[0] + resetColor + ": " + orange + timeInSubject + resetColor)

    total = minutes_to_str(total) if (humanMode) else  str(total)

    print( titleColor + "Total" + resetColor + ": " + orange + total + resetColor)
