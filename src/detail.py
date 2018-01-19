#!/usr/bin/env python3

import os
import subprocess
import sys
from timePerSubject import time_of_subjects
from timeToStr import minutes_to_str

if len(sys.argv) < 2:
    print ("you must pass a subject")
    sys.exit()

subject = sys.argv[1]

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


def gateway(params):
    prefix = [ os.path.dirname(os.path.realpath(__file__)) + '/gateway.sh']
    return subprocess.run(prefix + params, stdout=subprocess.PIPE).stdout.decode('UTF-8')


subjectRow = gateway(['listSubject', subject])
columns  = subjectRow.split('|')
lastTime=gateway(['daysSinceLastStudy', subject])

lastTime = "yesterday" if lastTime == "0" else lastTime + " days ago"

time_already_invested = time_of_subjects()
time_invested=minutes_to_str(time_already_invested[subject])

print(title + "Subject: " + section_color + subject + reset)
print (title + "Last time: " + reset + lastTime + reset )
print (title + "Time invested: " + reset + green + time_invested + reset )
print (title + "Importance: "  + reset + columns[1]  + reset )
print (title + "Complexity: " + reset  + columns[2]  + reset )
print ("")
print (orange + "To do next:" + reset)
print (columns[3])
print ("")
first = False

cmd = os.path.dirname(os.path.realpath(__file__)) + '/log.sh'
print( subprocess.run([cmd, '--filter', subject], stdout=subprocess.PIPE).stdout.decode('UTF-8'))


