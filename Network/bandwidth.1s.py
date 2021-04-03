#!/usr/bin/env -S PATH="${PATH}:/usr/local/bin" python3

# <xbar.title>Bandwidth.py</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>ChTBoner</xbar.author>
# <xbar.author.github>chtboner</xbar.author.github>
# <xbar.desc>Displays upload and download speeds</xbar.desc>
# <xbar.image>https://i.imgur.com/qDPtOxl.png</xbar.image>
# <xbar.dependencies>python3, psutil</xbar.dependencies>
#
# by ChTBoner

"""
    An implementation in python of the Bandwith Bitbar Plugin
    Should work on Linux too with Argos or other status bar (Polybar for example)
    https://getbitbar.com/plugins/Network/bandwidth.1s.sh

    Unlike the shell status bar, here he result displayed in the bar will the total of all interfaces
"""

import psutil
from time import sleep


def human_bytes(n):
    """
        convert bytes to human readable format
        'borrowed' from https://github.com/giampaolo/psutil/blob/master/scripts/ifconfig.py
    """

    symbols = ('K', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y')
    prefix = {}
    for i, s in enumerate(symbols):
        prefix[s] = 1 << (i + 1) * 10

    for symbol in reversed(symbols):
        if n >= prefix[symbol]:
            value = n / prefix[symbol]
            return '{} {}'.format(round(value, 2), symbol)
    return "{} B".format(round(n, 2))


def print_stats(upload, download):
    """
    pretty prints the results
    upload and download values are multiplied by 2 to compensate 0.5 second sleep
    """
    return "▼ {} - {} ▲".format(human_bytes(upload * 2), human_bytes(download * 2))


def main():
    # create dict to store results for every interface
    results = {}

    # set original data and store it in dict
    results.update(
        {'total': {
            'up': psutil.net_io_counters(pernic=False).bytes_sent,
            'down': psutil.net_io_counters(pernic=False).bytes_recv}
        })

    # loops through all interfaces to store upload and download data
    for interface in psutil.net_io_counters(pernic=True):
        results.update(
            {interface: {
                'up': psutil.net_io_counters(pernic=True)[interface].bytes_sent,
                'down': psutil.net_io_counters(pernic=True)[interface].bytes_recv}
            })

    sleep(0.5)

    # after sleeping, updates the data to be the difference between the two iterations
    results.update(
        {'total': {
            'up': psutil.net_io_counters(pernic=False).bytes_sent - results["total"]["up"],
            'down': psutil.net_io_counters(pernic=False).bytes_recv - results["total"]["down"]}
        })

    # iterates over interfaces to update data
    for interface in results:
        if interface != 'total':
            results.update({
                interface: {
                    'up': psutil.net_io_counters(pernic=True)[interface].bytes_sent - results[interface]["up"],
                    'down': psutil.net_io_counters(pernic=True)[interface].bytes_recv - results[interface]["down"]}
            })

    # print the result to the bar
    print(print_stats(results["total"]["up"], results["total"]["down"]))

    print('---')
    # print in dropdown
    for interface in results:
        if interface != 'total':
            print("{}: {}".format(interface, print_stats(results[interface]["up"], results[interface]["down"])))


if __name__ == '__main__':
    main()
