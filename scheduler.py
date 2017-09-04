#!/usr/bin/env python

import operator
import subprocess
import datetime

def gateway(params):
    return subprocess.run(params, stdout=subprocess.PIPE).stdout.decode('UTF-8')

subjects = gateway(['./gateway.sh', 'list_subjects_names'])


#build the initial dic
subjects_weights = {}
for entry in subjects.splitlines() :
    subjects_weights[entry] = 1

# change values based on the importance of the subject configured
for subject in subjects_weights:
    importance_str = gateway(['./gateway.sh', 'get_weight_by_name', subject])
    importance = int(importance_str)
    subjects_weights[subject]+= subjects_weights[subject] * (importance * 0.1)

# give less probability to the latest and more to the earlier
for subject in subjects_weights:
    days_since_last_study =  int(gateway(['./gateway.sh', 'days_since_last_study', subject]))
    subjects_weights[subject]+=  subjects_weights[subject] * days_since_last_study


# turns the last one less probable to repeat
last_entry =  gateway(['./gateway.sh', 'last_entry_name'])
subjects_weights[last_entry] = subjects_weights[last_entry] / 4

# give more probability to new subjects (which were never used)
new_subjects =  gateway(['./gateway.sh', 'new_subjects'])
for subject in new_subjects.splitlines() :
    subjects_weights[subject] =  subjects_weights[subject]  * 2

# calculate the energy levels
now = datetime.datetime.now()
#low energy level period
if now.hour > 22 or now.hour < 4:
    for subject in subjects_weights:
        energy_level = int(gateway(['./gateway.sh', 'get_energy_level_by_name', subject]))
        subjects_weights[subject] = subjects_weights[subject] * (1/energy_level)

def print_result(subjects_weights):
    sorted_subjects  = sorted(subjects_weights.items(), key=operator.itemgetter(1), reverse=True)
    for subject,weight in sorted_subjects:
        what_todo = gateway(['./gateway.sh', 'get_whattodo_details_by_name', subject])
        print (subject + ": " + what_todo)

print_result(subjects_weights)

