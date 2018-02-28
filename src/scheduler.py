#!/usr/bin/env python3

from functools import reduce
import json
import operator
import subprocess
import datetime
import sys
import math
import os
from timePerSubject import time_of_subjects
from timeToStr import minutes_to_str
from gateway import gateway

def factory_subjects():
    subjects = gateway(['listSubjects'])
    if len(subjects) < 10:
        print ("[]")
        sys.exit()
    invested_time_in_subjects = time_of_subjects()
    #build the initial dic
    subjects_configs = {}
    for line in subjects.splitlines() :
        columns  = line.split('|')
        days_str = gateway(['daysSinceLastStudy', columns[0]])
        if days_str == "":
            days_str="0"

        time_already_invested = invested_time_in_subjects[columns[0]] if columns[0] in invested_time_in_subjects  else 0

        subjects_configs[columns[0]] = {
                'name': columns[0],
                'weight': 0.5,
                'priority': int(columns[1]),
                'complexity': int(columns[2]),
                'whatToDoNext': columns[3],
                'objective': columns[4] if len(columns) > 4 else '',
                'days_since_last_study': int(days_str),
                'time_already_invested': time_already_invested,
                'time_already_invested_str': minutes_to_str(time_already_invested)
        }

    return subjects_configs


def alter_by_priority(subjects_configs):
    return subjects_configs


def reduce_base(obj, attr):
    value = obj[attr]
    obj[attr] = value / 100
    return obj

def increse_base(obj, attr):
    value = obj[attr]
    obj[attr] = int(value * 100)
    return obj


def configure_subjects(tiredMode=False):
    subjects_configs = factory_subjects()


    # importance factors
    # - longest without made
    # - high complexity
    # - high priority
    # - new
    # after doing a high complexity one it's better to get a lighter one

    # cost factors
    # - low priority
    # - recently made

    # decrease the bases to work between 1 and 0
    subjects_configs = {k: reduce_base(v, "priority") for k, v in subjects_configs.items()}


    # change values based on the importance of the subject configured
    for subject in subjects_configs:
        subjects_configs[subject]['weight'] += subjects_configs[subject]['weight'] * (math.pow(subjects_configs[subject]['priority'], 2) * 0.9)


    # give less probability to the latest and more to the earlier
    for subject in subjects_configs:
        if subjects_configs[subject]['days_since_last_study'] == "":
            subjects_configs[subject]['weight'] = pow(2, subjects_configs[subject]['weight'])
            continue
        days_since_last_study =  int(subjects_configs[subject]['days_since_last_study'])
        subjects_configs[subject]['weight'] = pow(days_since_last_study, 2)


    # give more probability to new subjects (which were never used)
    new_subjects =  gateway(['new_subjects'])
    for subject in new_subjects.splitlines() :
        subjects_configs[subject]['weight'] =  subjects_configs[subject]['weight']

    # -- contextual calculai in the end --

    if tiredMode:
        for subject in subjects_configs:
            base = (1 / math.pow(subjects_configs[subject]['complexity'],9))
            subjects_configs[subject]['weight'] = subjects_configs[subject]['weight'] *  base 

    subjects_configs = {k: increse_base(v, "priority") for k, v in subjects_configs.items()}

    return sort_subjects(subjects_configs)

def sort_subjects(configured_subjects):

    subjects_list = []
    for key, value in configured_subjects.items():
        subjects_list.append(value)

    return sorted(subjects_list, key=lambda x: x['weight'], reverse=True)


def print_cli(subjects):
    orange = os.getenv('WN_COLOR_ORANGE').encode('utf-8').decode('unicode_escape')
    green = os.getenv('WN_COLOR_GREEN').encode('utf-8').decode('unicode_escape')
    red = os.getenv('WN_COLOR_RED').encode('utf-8').decode('unicode_escape')
    reset = os.getenv('WN_COLOR_RESET').encode('utf-8').decode('unicode_escape')
    title = os.getenv('WN_COLOR_TITLE').encode('utf-8').decode('unicode_escape')
    section_color = os.getenv('WN_COLOR_SECTION').encode('utf-8').decode('unicode_escape')

    if os.environ.get('NO_COLOR') is not None:
        orange=''
        green=''
        red=''
        reset=''
        title=''


    counter = 1
    first = True
    for subject in subjects:
        days_since_last_study = subject['days_since_last_study']

        if days_since_last_study < 7:
            daysColor = green
        elif days_since_last_study > 7 and days_since_last_study < 30:
            daysColor = orange
        else:
            daysColor = red

        daysColor = orange
        days_since_last_study_str = ""
        if days_since_last_study > 0:
            days_since_last_study_str =  daysColor  + str(days_since_last_study) + ' days ago'


        time_invested = green + subject['time_already_invested_str'] +  reset

        time_invested = " - " + time_invested  if len(time_invested) > 4 else "" 

        print ( title + str(counter) + '. ' + subject['name'] + reset + ': ' + days_since_last_study_str + reset + time_invested + reset + ' ' + subject['whatToDoNext'] + reset)
        counter += 1

# -- printing ---
if __name__ == '__main__':
    if os.environ.get('TIRED') is not None:
        subjects = configure_subjects(True)
    else:
        subjects = configure_subjects(False)


    if os.environ.get('TO_JSON') is not None:
        if not subjects:
            print("[]")
        else:
            print(json.dumps(subjects))

        sys.exit()

    print_cli(subjects)
