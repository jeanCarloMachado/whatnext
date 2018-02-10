#!/usr/bin/env python3

import os
import subprocess
import sys
from timePerSubject import time_of_subjects
from timeToStr import minutes_to_str
from gateway import gateway, gatewaySuccess
import json

def get_subject(name):
    subject = {}
    subject['name']  = name
    if not gatewaySuccess(['subjectExists', subject['name']]):
        print ("you must pass a valid subject")
        sys.exit()

    subjectRow = gateway(['listSubject', subject['name']])
    columns  = subjectRow.split('|')
    days_since_last_study=gateway(['daysSinceLastStudy', subject['name']])
    days_since_last_study = days_since_last_study or 0

    subject['priority'] = int(columns[1])
    subject['complexity'] = int(columns[2])
    subject['what_to_do_next'] = columns[3]
    subject['days_since_last_study'] = int(days_since_last_study)
    time_already_invested_of_subjects = time_of_subjects()

    subject['time_already_invested']  = time_already_invested_of_subjects[subject['name']] if subject['name'] in time_already_invested_of_subjects else 0
    subject['time_already_invested_str'] = minutes_to_str(subject['time_already_invested'])


    cmd = os.path.dirname(os.path.realpath(__file__)) + '/log.sh'
    historyStr = ( subprocess.run([cmd, '--filter', subject['name'], '--json'], stdout=subprocess.PIPE).stdout.decode('UTF-8'))
    subject['history']  = json.loads(historyStr)

    return subject

if __name__ == '__main__':

    subject=get_subject(sys.argv[1])

    if os.environ.get('TO_JSON') is not None:
        print(json.dumps(subject))
        sys.exit()

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

    days_since_last_study = "yesterday" if subject['days_since_last_study'] == 0 else str(subject['days_since_last_study']) + " days ago"

    print(title + "Subject: " + section_color + subject['name'] + reset)
    print (title + "Last time: " + reset + days_since_last_study + reset )
    print (title + "Time invested: " + reset + green + subject['time_already_invested_str']  + reset )
    print (title + "Importance: "  + reset + subject['priority']  + reset )
    print (title + "Complexity: " + reset  + subject['complexity']  + reset )
    print ("")
    print (orange + "To do next:" + reset)
    print (subject['what_to_do_next'])
    print ("")
    cmd = os.path.dirname(os.path.realpath(__file__)) + '/log.sh'
    print( subprocess.run([cmd, '--filter', subject['name']], stdout=subprocess.PIPE).stdout.decode('UTF-8'))

