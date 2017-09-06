#!/usr/bin/env python

import operator
import subprocess
import datetime
import sys
import math

def gateway(params):
    prefix = ['./gateway.sh']
    return subprocess.run(prefix + params, stdout=subprocess.PIPE).stdout.decode('UTF-8')

class Subject:
    def __init__(self, name, weight, priority, energy_level, what_to_do_next):
        self.name = name
        self.weight = weight
        self.priority = priority
        self.energy_level = energy_level
        self.what_to_do_next = what_to_do_next


def factory_subjects():
    subjects = gateway(['list_subjects'])
    #build the initial dic
    subjects_configs = {}
    for line in subjects.splitlines() :
        columns  = line.split('|')
        subjects_configs[columns[0]] = Subject(name=columns[0], weight=1, priority=int(columns[1]), energy_level=int(columns[2]), what_to_do_next=columns[3])

    return subjects_configs


subjects_configs = factory_subjects()

# change values based on the importance of the subject configured
def configure_importance(subjects_configs):
    for subject in subjects_configs:
        subjects_configs[subject].weight += subjects_configs[subject].weight * (math.pow(subjects_configs[subject].priority,2) * 0.5)

    return subjects_configs

subjects_configs = configure_importance(subjects_configs)

# give less probability to the latest and more to the earlier
for subject in subjects_configs:
    days_since_last_study =  int(gateway(['days_since_last_study', subject]))
    subjects_configs[subject].weight +=  subjects_configs[subject].weight * days_since_last_study

# turns the last one less probable to repeat
last_entry =  gateway(['last_entry_name'])
subjects_configs[last_entry].weight = subjects_configs[last_entry].weight / 4

# give more probability to new subjects (which were never used)
new_subjects =  gateway(['new_subjects'])
for subject in new_subjects.splitlines() :
    subjects_configs[subject].weight =  subjects_configs[subject].weight * 2

# -- contextual calculai in the end --

# calculate the energy levels
now = datetime.datetime.now()
#low energy level period
if now.hour > 22 or now.hour < 4:
    for subject in subjects_configs:
        subjects_configs[subject].weight = subjects_configs[subject].weight * (1/subjects_configs[subject].energy_level)
# -- printing ---

def print_result(subjects_configs):
    sorted_subjects  = sorted(subjects_configs.items(), key=lambda x: x[1].weight, reverse=True)
    for subject,weight in sorted_subjects:
        what_todo = gateway(['get_whattodo_details_by_name', subject])
        last_date_studied = gateway(['last_studied_date_for_subject', subject])
        print ('\x1b[7;30;42m' + subject + '\x1b[0m' + '|' + what_todo + '|' + '\x1b[7;30;43m' + last_date_studied + '\x1b[0m' )

print_result(subjects_configs)
