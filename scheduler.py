#!/usr/bin/env python3

import operator
import subprocess
import datetime
import sys
import math
import os

def memoize(function):
    from functools import wraps
    memo = {}
    @wraps(function)
    def wrapper(*args):
        if str(args) in memo:
            return memo[str(args)]
        else:
            rv = function(*args)
            memo[str(args)] = rv
            return rv
    return wrapper

@memoize
def gateway(params):
    prefix = [ os.path.dirname(os.path.realpath(__file__)) + '/gateway.sh']
    return subprocess.run(prefix + params, stdout=subprocess.PIPE).stdout.decode('UTF-8')

class Subject:
    def __init__(self, name, weight, priority, complexity, whatToDoNext, daysSinceLastStudy):
        self.name = name
        self.weight = weight
        self.priority = priority
        self.complexity = complexity
        self.whatToDoNext = whatToDoNext
        self.daysSinceLastStudy = daysSinceLastStudy

def factory_subjects():
    subjects = gateway(['listSubjects'])
    #build the initial dic
    subjects_configs = {}
    for line in subjects.splitlines() :
        columns  = line.split('|')
        subjects_configs[columns[0]] = Subject(
                name=columns[0],
                weight=1,
                priority=int(columns[1]),
                complexity=int(columns[2]),
                whatToDoNext=columns[3],
                daysSinceLastStudy=gateway(['daysSinceLastStudy', columns[0]])
        )

    return subjects_configs


subjects_configs = factory_subjects()

# change values based on the importance of the subject configured
def configure_importance(subjects_configs):
    for subject in subjects_configs:
        subjects_configs[subject].weight += subjects_configs[subject].weight * (math.pow(subjects_configs[subject].priority, 2) * 0.9)

    return subjects_configs

subjects_configs = configure_importance(subjects_configs)

# give less probability to the latest and more to the earlier
for subject in subjects_configs:
    if subjects_configs[subject].daysSinceLastStudy == "":
        subjects_configs[subject].weight = (subjects_configs[subject].weight  * subjects_configs[subject].weight * 0.2)
        continue
    daysSinceLastStudy =  int(subjects_configs[subject].daysSinceLastStudy)
    subjects_configs[subject].weight +=  subjects_configs[subject].weight * daysSinceLastStudy

# turns the last one less probable to repeat
last_entry =  gateway(['lastEntryName'])
if last_entry in subjects_configs:
    subjects_configs[last_entry].weight = subjects_configs[last_entry].weight / 4

# give more probability to new subjects (which were never used)
new_subjects =  gateway(['new_subjects'])
for subject in new_subjects.splitlines() :
    subjects_configs[subject].weight =  subjects_configs[subject].weight * 1.1

# -- contextual calculai in the end --

now = datetime.datetime.now()
#low energy level period
if (now.hour > 22 or now.hour < 4) or os.environ.get('TIRED') is not None:
    for subject in subjects_configs:
        base = (1 / math.pow(subjects_configs[subject].complexity,9))
        subjects_configs[subject].weight = subjects_configs[subject].weight *  base 
# -- printing ---

orange = os.getenv('WN_COLOR_ORANGE').encode('utf-8').decode('unicode_escape')
green = os.getenv('WN_COLOR_GREEN').encode('utf-8').decode('unicode_escape')
red = os.getenv('WN_COLOR_RED').encode('utf-8').decode('unicode_escape')
reset = os.getenv('WN_COLOR_RESET').encode('utf-8').decode('unicode_escape')
title = os.getenv('WN_COLOR_TITLE').encode('utf-8').decode('unicode_escape')

def print_result(subjects_configs):
    sorted_subjects  = sorted(subjects_configs.items(), key=lambda x: x[1].weight, reverse=True)
    for subject,weight in sorted_subjects:
        daysSinceLastStudyStr = subjects_configs[subject].daysSinceLastStudy
        daysSinceLastStudyInt = int(daysSinceLastStudyStr) if daysSinceLastStudyStr.isdigit() else 0

        if daysSinceLastStudyInt < 7:
            daysColor = green
        elif daysSinceLastStudyInt > 7 and daysSinceLastStudyInt < 20:
            daysColor = orange
        else:
            daysColor = red

        if daysSinceLastStudyStr == '':
            daysSinceLastStudyStr = 'never '
            daysColor = orange
        else:
            daysSinceLastStudyStr+=  ' days ago '

        print (title + ' ' + subject + reset + daysColor + ' ' + daysSinceLastStudyStr + reset + '' + subjects_configs[subject].whatToDoNext + reset)

print_result(subjects_configs)
