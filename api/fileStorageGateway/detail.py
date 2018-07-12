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
    if not gatewaySuccess(['subjectExists', name]):
        print ('Subject "'+ name + '" was not found')
        sys.exit()

    subjectRow = gateway(['subjectJson', subject['name']])

    subject = json.loads(subjectRow)

    days_since_last_study=gateway(['daysSinceLastStudy', subject['name']])
    days_since_last_study = days_since_last_study or 0
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
    print(json.dumps(subject))
