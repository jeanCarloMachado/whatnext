#!/usr/bin/env python3
import sys

intervals = (
    ('weeks', 604800),  # 60 * 60 * 24 * 7
    ('days', 86400),    # 60 * 60 * 24
    ('hours', 3600),    # 60 * 60
    ('minutes', 60),
    ('seconds', 1),
    )

granularity = 2


def seconds_to_str(seconds):
    result = []
    for name, count in intervals:
        value = seconds // count
        if value:
            seconds -= value * count
            if value == 1:
                name = name.rstrip('s')
            result.append("{} {}".format(value, name))
    return ', '.join(result[:granularity])


def minutes_to_str(minutes):
    seconds = minutes * 60
    return seconds_to_str(seconds)


if __name__ == '__main__':
    if len(sys.argv) == 1:
        time = int(sys.stdin.readlines()[0])
    else:
        time = int(sys.argv[1])

    if len(sys.argv) > 2:
        if sys.argv[2] == "--from-minutes" :
            print(minutes_to_str(time))
            sys.exit(0)

    print (seconds_to_str(time))
