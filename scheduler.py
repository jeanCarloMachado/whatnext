#!/usr/bin/env python3

import operator
import subprocess
import datetime
import sys
import math
import os
from timePerSubject import time_of_subjects
from timeToStr import minutes_to_str

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
    def __init__(self, name, weight, priority, complexity, what_to_do_next, days_since_last_study, time_already_invested):
        self.name = name
        self.weight = weight
        self.priority = priority
        self.complexity = complexity
        self.what_to_do_next = what_to_do_next
        self.days_since_last_study = days_since_last_study
        self.time_already_invested = time_already_invested


def factory_subjects():
    subjects = gateway(['listSubjects'])
    time_already_invested = time_of_subjects()
    #build the initial dic
    subjects_configs = {}
    for line in subjects.splitlines() :
        columns  = line.split('|')
        subjects_configs[columns[0]] = Subject(
                name=columns[0],
                weight=1,
                priority=int(columns[1]),
                complexity=int(columns[2]),
                what_to_do_next=columns[3],
                days_since_last_study=gateway(['daysSinceLastStudy', columns[0]]),
                time_already_invested=time_already_invested[columns[0]] if columns[0] in time_already_invested  else 0
        )

    return subjects_configs


# change values based on the importance of the subject configured
def configure_importance(subjects_configs):
    for subject in subjects_configs:
        subjects_configs[subject].weight += subjects_configs[subject].weight * (math.pow(subjects_configs[subject].priority, 2) * 0.9)

    return subjects_configs

def configure_subjects():
    subjects_configs = factory_subjects()
    subjects_configs = configure_importance(subjects_configs)

    # give less probability to the latest and more to the earlier
    for subject in subjects_configs:
        if subjects_configs[subject].days_since_last_study == "":
            subjects_configs[subject].weight = (subjects_configs[subject].weight  * subjects_configs[subject].weight * 0.2)
            continue
        days_since_last_study =  int(subjects_configs[subject].days_since_last_study)
        subjects_configs[subject].weight += subjects_configs[subject].weight * days_since_last_study

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
    #tired period
    if (now.hour > 22 or now.hour < 4) or os.environ.get('TIRED') is not None:
        for subject in subjects_configs:
            base = (1 / math.pow(subjects_configs[subject].complexity,9))
            subjects_configs[subject].weight = subjects_configs[subject].weight *  base 

    return subjects_configs

# -- printing ---

orange = os.getenv('WN_COLOR_ORANGE').encode('utf-8').decode('unicode_escape')
green = os.getenv('WN_COLOR_GREEN').encode('utf-8').decode('unicode_escape')
red = os.getenv('WN_COLOR_RED').encode('utf-8').decode('unicode_escape')
reset = os.getenv('WN_COLOR_RESET').encode('utf-8').decode('unicode_escape')
title = os.getenv('WN_COLOR_TITLE').encode('utf-8').decode('unicode_escape')

if os.environ.get('NO_COLOR') is not None:
    orange=''
    green=''
    red=''
    reset=''
    title=''


def get_days_since_last_study_str(subject):
    days_since_last_study_str = subject.days_since_last_study
    days_since_last_study_int = int(days_since_last_study_str) if days_since_last_study_str.isdigit() else 0

    if days_since_last_study_int < 7:
        daysColor = green
    elif days_since_last_study_int > 7 and days_since_last_study_int < 20:
        daysColor = orange
    else:
        daysColor = red

    if days_since_last_study_str == '':
        days_since_last_study_str = ''
        daysColor = orange
    else:
        days_since_last_study_str =  daysColor  + days_since_last_study_str + ' days ago' 

    return days_since_last_study_str

def print_result(subjects_configs):
    sorted_subjects  = sorted(subjects_configs.items(), key=lambda x: x[1].weight, reverse=True)
    for subject,weight in sorted_subjects:
        days_since_last_study_str = get_days_since_last_study_str(subjects_configs[subject])
        time_invested = minutes_to_str(subjects_configs[subject].time_already_invested)
        time_invested = " - " + time_invested  if len(time_invested) > 4 else "" 

        print (title + ' ' + subject + reset + ' (' + days_since_last_study_str + '' + time_invested + reset + ') ' + subjects_configs[subject].what_to_do_next + reset)

print_result(configure_subjects())
