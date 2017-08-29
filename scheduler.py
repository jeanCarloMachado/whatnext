#!/usr/bin/env python

import operator
import subprocess

def gateway(params):
    return subprocess.run(params, stdout=subprocess.PIPE).stdout.decode('UTF-8')

subjects = gateway(['./gateway.sh', 'list_subjects_names'])


#build the initial dic
subjects_weights = {}
for entry in subjects.splitlines() :
    subjects_weights[entry] = 0

# give less probability to the latest and more to the earlier
from_last_to_earlier =  gateway(['./gateway.sh', 'unique_occurence_from_last_to_earlier'])
counter = 0
for subject in from_last_to_earlier.splitlines() :
    counter  = counter + 1
    subjects_weights[subject] +=  (counter * 2)


# turns the last one less probable to repeat
last_entry =  gateway(['./gateway.sh', 'last_entry_name'])
subjects_weights[last_entry] -= 10

# give more probability to new subjects (which were never used)
new_subjects =  gateway(['./gateway.sh', 'new_subjects'])
for subject in new_subjects.splitlines() :
    subjects_weights[subject] +=  10

# change values based on the importance of the subject configured
for subject in subjects_weights:
    importance = int(gateway(['./gateway.sh', 'get_weight_by_name', subject]))
    subjects_weights[subject] += importance


sorted_subjects  = sorted(subjects_weights.items(), key=operator.itemgetter(1), reverse=True)
for  subject, weight in sorted_subjects:
    print (subject + ": " + str(weight))

