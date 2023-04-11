#!/usr/bin/env python3

# <xbar.title>Countdown</xbar.title>
# <xbar.version>v2.1</xbar.version>
# <xbar.author>Pere Albujer</xbar.author>
# <xbar.author.github>P4R</xbar.author.github>
# <xbar.desc>Shows countdown of established date.</xbar.desc>
# <xbar.image>https://cloud.githubusercontent.com/assets/7404532/12356787/ae62636c-bba4-11e5-8ff8-6a1eaffcbfc2.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.var>string(VAR_TITLE="Countdown Timer"): Title to display in menu bar</xbar.var>
# <xbar.var>string(VAR_DATE_FORMAT="%d-%m-%Y"): The date / time format for the timers you are setting up</xbar.var>
# <xbar.var>boolean(VAR_NO_CYCLE=false): If true, times will not cycle in the menu bar but will be displayed in the drop-down menu</xbar.var>
# <xbar.var>string(VAR_TIMERS="Time #1:17-07-2073,Time #2:15-08-2073"): Comma delimited list of timers you wish to set. Each list entry is a colon separated label and date / time matching the format set in the DATE_FORMAT variable.</xbar.var>

from datetime import datetime
import sys
import os


def dateDiffInSeconds(date1, date2):
    timedelta = date2 - date1
    return timedelta.days * 24 * 3600 + timedelta.seconds


def daysHoursMinutesSecondsFromSeconds(seconds):
    minutes, seconds = divmod(seconds, 60)
    hours, minutes = divmod(minutes, 60)
    days, hours = divmod(hours, 24)
    return (days, hours, minutes)


def main():
    bar_title = os.environ.get("VAR_TITLE")
    date_format = os.environ.get("VAR_DATE_FORMAT")
    no_cycle = os.environ.get("VAR_NO_CYCLE")
    timers = os.environ.get("VAR_TIMERS").split(",")

    now = datetime.now()
    time = None

    if no_cycle == "false":
        print(bar_title + " | font=\'Monospace\'")
        print("---")

    for timer in timers:
        timerData = timer.split(":")
        label = timerData[0]
        timer_date = timerData[1]

        try:
            time = datetime.strptime(timer_date, date_format)
            print(label + ": %d d, %d h, %d m | font=\'Monospace\'" % daysHoursMinutesSecondsFromSeconds(dateDiffInSeconds(now, time)))
        except ValueError as e:
            print(e)


if __name__ == "__main__":
    main()
