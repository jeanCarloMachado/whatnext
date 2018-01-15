#!/usr/bin/env python3
import sys

time=0
timeMultiplier=1

if len(sys.argv) == 1:
    time = int(sys.stdin.readlines()[0])
else:
    time = int(sys.argv[1])

if len(sys.argv) > 2:
    if sys.argv[2] == "--from-minutes" :
        timeMultiplier = 60

time = time * timeMultiplier


intervals = (
    ('weeks', 604800),  # 60 * 60 * 24 * 7
    ('days', 86400),    # 60 * 60 * 24
    ('hours', 3600),    # 60 * 60
    ('minutes', 60),
    ('seconds', 1),
    )

def display_time(seconds, granularity=2):
    result = []

    for name, count in intervals:
        value = seconds // count
        if value:
            seconds -= value * count
            if value == 1:
                name = name.rstrip('s')
            result.append("{} {}".format(value, name))
    return ', '.join(result[:granularity])

print (display_time(time))
