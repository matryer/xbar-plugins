#!/usr/bin/python

# <bitbar.title>Fuzzy Clock</bitbar.title>
# <bitbar.author>Dylan Evans</bitbar.author>
# <bitbar.author.github>whonut</bitbar.author.github>
# <bitbar.desc>Display the current system time in a 'fuzzy' manner, rounding to the nearest 5 minutes and using words.</bitbar.desc>
# <bitbar.version>1.0</bitbar.version>
#
# 1 second refresh rate may be overkill. Wording & formatting of the time may
# also be easily altered below.

from __future__ import absolute_import, division, print_function, unicode_literals
from time import localtime


def round_to_nearest_five(n):
    '''Round the float n to the nearest 5.'''
    return int(5 * round(n / 5))


def next_hour(hour):
    # modulo before adding one so that 11 => 12 and not 0
    return (hour % 12) + 1


def fuzzy_time(struct_time):
    '''Return the current 'fuzzy time' (rounded to the nearest 5 minutes) as a
       string.'''

    # Split it into hours & minutes and rounding the minutes to make the time
    # 'fuzzy'. Use 12-hour clock.
    hour = (struct_time.tm_hour % 12) or 12
    minute = struct_time.tm_min + (struct_time.tm_sec / 60)
    rounded_min = round_to_nearest_five(minute)
    if rounded_min == 60:
        rounded_min = 0
        hour = next_hour(hour)

    num_word = {1: "one", 2: "two", 3: "three", 4: "four", 5: "five", 6: "six",
                7: "seven", 8: "eight", 9: "nine", 10: "ten", 11: "eleven",
                12: "twelve", 20: "twenty", 25: "twenty-five"}

    # Work out what to display and display it.
    if rounded_min == 0:
        return "{hr} o'clock".format(hr=num_word[hour])
    elif rounded_min == 15:
        return "quarter past {hr}".format(hr=num_word[hour])
    elif rounded_min < 30:
        return "{min} past {hr}".format(min=num_word[rounded_min],
                                        hr=num_word[hour])
    elif rounded_min == 30:
        return "half past {hr}".format(hr=num_word[hour])
    elif rounded_min == 45:
        return "quarter to {hr}".format(hr=num_word[next_hour(hour)])
    else:
        return "{min} to {hr}".format(min=num_word[60-rounded_min],
                                      hr=num_word[next_hour(hour)])

if __name__ == '__main__':
    print(fuzzy_time(localtime()))
