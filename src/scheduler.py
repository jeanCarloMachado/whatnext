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
    intested_time_of_subjects = time_of_subjects()
    #build the initial dic
    subjects_configs = {}
    for line in subjects.splitlines() :
        columns  = line.split('|')
        days_str = gateway(['daysSinceLastStudy', columns[0]])
        if days_str == "":
            days_str="0"

        time_already_invested = intested_time_of_subjects[columns[0]] if columns[0] in intested_time_of_subjects  else 0

        subjects_configs[columns[0]] = {
                'name': columns[0],
                'weight': 1,
                'priority': int(columns[1]),
                'complexity': int(columns[2]),
                'what_to_do_next': columns[3],
                'objective': columns[4] if len(columns) > 3 else "",
                'days_since_last_study': int(days_str),
                'time_already_invested': time_already_invested,
                'time_already_invested_str': minutes_to_str(time_already_invested)
        }

    return subjects_configs


# change values based on the importance of the subject configured
def alter_by_priority(subjects_configs):
    for subject in subjects_configs:
        subjects_configs[subject]['weight'] += subjects_configs[subject]['weight'] * (math.pow(subjects_configs[subject]['priority'], 2) * 0.9)

    return subjects_configs

def configure_subjects(tiredMode=False):
    subjects_configs = factory_subjects()
    subjects_configs = alter_by_priority(subjects_configs)

    # give less probability to the latest and more to the earlier
    for subject in subjects_configs:
        if subjects_configs[subject]['days_since_last_study'] == "":
            subjects_configs[subject]['weight'] = (subjects_configs[subject]['weight']  * subjects_configs[subject]['weight'] * 0.2)
            continue
        days_since_last_study =  int(subjects_configs[subject]['days_since_last_study'])
        subjects_configs[subject]['weight'] += subjects_configs[subject]['weight'] * days_since_last_study

    # turns the last one less probable to repeat
    last_entry =  gateway(['lastEntryName'])
    if last_entry in subjects_configs:
        subjects_configs[last_entry]['weight'] = subjects_configs[last_entry]['weight'] / 4

    # give more probability to new subjects (which were never used)
    new_subjects =  gateway(['new_subjects'])
    for subject in new_subjects.splitlines() :
        subjects_configs[subject]['weight'] =  subjects_configs[subject]['weight'] * 2.1

    # -- contextual calculai in the end --

    if tiredMode:
        for subject in subjects_configs:
            base = (1 / math.pow(subjects_configs[subject]['complexity'],9))
            subjects_configs[subject]['weight'] = subjects_configs[subject]['weight'] *  base 

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

        print ( title + str(counter) + '. ' + subject['name'] + reset + ': ' + days_since_last_study_str + reset + time_invested + reset + ' ' + subject['what_to_do_next'] + reset)
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
