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

def configure_subjects(tiredMode=False):

    CLI_PATH = os.path.dirname(os.path.abspath(__file__))
    cmd = [
        CLI_PATH + '/Scheduler',
    ]

    content = subprocess.run(cmd,  stdout=subprocess.PIPE).stdout.decode('UTF-8')

    return json.loads(content)


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


        time_invested = green + str(subject['time_already_invested']) + " minutes " +  reset

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
