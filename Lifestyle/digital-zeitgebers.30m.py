#!/usr/bin/python

# <bitbar.title>Digital Zeitgebers</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Edward Qiu</bitbar.author>
# <bitbar.author.github>edwardqiu</bitbar.author.github>
# <bitbar.desc>A fuzzy clock that reminds you of certain sleep hyigene recommendations, such as when to avoid blue light, caffeine, etc., at the proper time.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/edwardqiu/digital-zeitgebers/master/docs/banner.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/edwardqiu/digital-zeitgebers/</bitbar.abouturl>

from time import localtime, strftime
from datetime import timedelta

""" User Settings - Change the values below"""
bedtime = 22 # Change the value to what time you usually go to bed in the 24hr format rounded to the nearest hour (i.e. 22 for 10:15pm)
wakeup_time = 6 # Change the value to what time you usually wake up in the 24hr format rounded to the nearest hour (i.e. 6 for 5:45am)
""" End of User Settings """

def generate_message_and_link():
    current_hour_military_time = int(strftime('%H', localtime()))
    message = ""
    link = ""
    topic = ""
    bedtime_delta = (timedelta(hours=bedtime) - timedelta(hours=current_hour_military_time)).seconds / 60 / 60
    wakeup_time_delta = (timedelta(hours=wakeup_time) - timedelta(hours=current_hour_military_time)).seconds / 60 / 60
    if current_hour_military_time == wakeup_time or wakeup_time_delta == 1:
        message = ":alarm_clock: Good morning! Time to get up!"
        topic = "Chronotypes and Circadian Rhythms"
        link = "http://www.scielo.br/pdf/bjmbr/v41n10/7132.pdf"
    elif bedtime_delta <= 6 and bedtime_delta > 2:
        message = ":coffee: No more caffeine!"
        topic = "Caffeine's effect on sleep"
        link = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3805807/"
    elif bedtime_delta == 2:
        message = ":no_mobile_phones: Avoid blue light!"
        topic = "Blue light exposure's effect on sleep"
        link = "https://www.sciencedirect.com/topics/agricultural-and-biological-sciences/chronotype#B9781437717037100301-p0155"
    elif bedtime_delta == 1:
        message = ":zzz: Prepare to sleep!"
        topic = "Sleep Latency"
        link = "https://www.sciencedirect.com/topics/agricultural-and-biological-sciences/chronotype#S0065266017300202-p0160"
    elif bedtime == current_hour_military_time:
        message = ":sleeping: Good night! Time for bed!"
        topic = "Chronotypes and Circadian Rhythms"
        link = "http://www.scielo.br/pdf/bjmbr/v41n10/7132.pdf"
    return [message, topic, link]


if __name__ == '__main__':
    delta = (timedelta(hours=bedtime) - (timedelta(hours=wakeup_time))).seconds / 60 / 60
    if not (bedtime >= 0 and bedtime <= 23):
        print(":heavy_exclamation_mark: Please input an hour that is between 0-23 for your bedtime")
    elif not (wakeup_time >= 0 and wakeup_time <= 23):
        print(":heavy_exclamation_mark: Please input an hour that is between 0-23 for your wakeup time")
    elif bedtime == wakeup_time:
        print(":heavy_exclamation_mark: Your bedtime and wakeup time cannot be the same value")
    elif delta > 17:
        print(":heavy_exclamation_mark: The interval between bedtime and wakeup time cannot be more than 17 hours.")
    else:
        outputs = generate_message_and_link()
        print("{state}".format(state=outputs[0]))
        print("---")
        print("{topic}".format(topic=outputs[1]))
        print("--Learn More | href={link}".format(link=outputs[2]))
