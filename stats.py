#!/usr/bin/env python

import operator
import subprocess
import datetime
import sys
import math

def gateway(params):
    prefix = ['gateway.sh']
    return subprocess.run(prefix + params, stdout=subprocess.PIPE).stdout.decode('UTF-8')


history = gateway(['listHistory'])


subjectsConfigs = {}
subjects = gateway(['listHistory'])
#build the initial dic
subjectData = {}
for line in subjects.splitlines() :
    columns  = line.split('|')
    name=columns[1]
    subjectData[name] = subjectData[name]+50 if name in subjectData else 50;


sortedSubjects = sorted(subjectData.items(), key=lambda value: value[1], reverse=True)

titleColor='\x1b[1;35;40m'
resetColor='\x1b[0m'
for item in sortedSubjects:
    print( titleColor + item[0] + resetColor )
    print ('    Minutes: ' + str(item[1]))

