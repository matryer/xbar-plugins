#!/usr/bin/env python

# <bitbar.title>Countdown</bitbar.title>
# <bitbar.version>v2.0</bitbar.version>
# <bitbar.author>Pere Albujer</bitbar.author>
# <bitbar.author.github>P4R</bitbar.author.github>
# <bitbar.desc>Shows countdown of established date.</bitbar.desc>
# <bitbar.image>https://cloud.githubusercontent.com/assets/7404532/12356787/ae62636c-bba4-11e5-8ff8-6a1eaffcbfc2.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>


from datetime import datetime
import sys
from exceptions import ValueError


def dateDiffInSeconds(date1, date2):
    timedelta = date2 - date1
    return timedelta.days * 24 * 3600 + timedelta.seconds


def daysHoursMinutesSecondsFromSeconds(seconds):
    minutes, seconds = divmod(seconds, 60)
    hours, minutes = divmod(minutes, 60)
    days, hours = divmod(hours, 24)
    return (days, hours, minutes)


def main():

    if "--help" in sys.argv:
        print(
            """
To pass arguments to this script, you can create a separate sh file and execute the main script with it.

Available Args:
    --bar-title: This will appear as the first line in the output. The default is 'Countdown Timer'.
    --date-format: You can provide a custom date format. The default is '%d-%m-%Y %H:%M'
    --no-cycle: If this is present in the arguments, the times will not cycle.
    --help: Prints this message and exits.

Example:
    countdown.py "--bar-title" "Custom Bar Title" "--no-cycle" "--date-format" "%d-%m-%Y" "Time #1" "17-07-2017" "Time #2" "15-08-2017"
Script Example:
    chmod +x /Path/to/countdown.py && /Path/to/countdown.py "--bar-title" "Custom Bar Title" "--no-cycle" "--date-format" "%d-%m-%Y" "Time #1" "17-07-2017" "Time #2" "15-08-2017"
            """
        )
        return

    arg_count = len(sys.argv)
    now = datetime.now()
    date_format = '%d-%m-%Y %H:%M'
    bar_title = "Countdown Timer"

    label = ""
    time = None

    if "--bar-title" in sys.argv:
        found_index = sys.argv.index("--bar-title")
        if len(sys.argv) > found_index + 1:
            bar_title = sys.argv[found_index + 1]

    if "--date-format" in sys.argv:
        found_index = sys.argv.index("--date-format")
        if len(sys.argv) > found_index + 1:
            date_format = sys.argv[found_index + 1]

    print(bar_title + " | font=\'Monospace\'")
    if "--no-cycle" in sys.argv:
        print("---")

    if arg_count == 1:
        print("""
            Please pass the correct arguments for this plugin to work.
            You can create an sh file that executes the main Python
            script file with the appropriate arguments.
            For examples, see the script file.
        """)

    for index in range(1, arg_count):
        arg = sys.argv[index].strip()
        if arg == "--no-cycle":
            continue

        if arg == "--bar-title":
            continue

        if index > 0 and sys.argv[index - 1] == "--date-format":
            continue

        try:
            time = datetime.strptime(arg, date_format)
            print(label + ": %d d, %d h, %d m | font=\'Monospace\'" % daysHoursMinutesSecondsFromSeconds(dateDiffInSeconds(now, time)))
        except ValueError:
            label = arg


if __name__ == "__main__":
    main()
