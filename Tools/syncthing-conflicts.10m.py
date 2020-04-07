#!/usr/bin/env python3
"""
    syncthing-conflicts.10m.py
    Python 2 and 3 compatible.

    Searches in your Syncthing shares to find conflicting files.
    Clicking on a conflict in the menu will open the enclosing folder in the Finder.

    'st' Menu item will turn red if there are any conflicts.

    This is a Bitbar plugin for MacOS
    https://github.com/matryer/bitbar

    <bitbar.title>Syncthing-conflicts</bitbar.title>
    <bitbar.version>v1.0</bitbar.version>
    <bitbar.author>Tim Battersby</bitbar.author>
    <bitbar.author.github>uglygus</bitbar.author.github>
    <bitbar.desc>Bitbar - Syncthing conflicts helper.</bitbar.desc>
    <bitbar.dependencies>python</bitbar.dependencies>
    <bitbar.image>https://i.imgur.com/CbthLRt.png</bitbar.image>

"""

import os
import re
import argparse
import sys
from subprocess import call


def find_conflicts():
    """ Reads a folder list from config.xml
        Returns a list of conflict found in those folders.
    """
    config_file = os.environ['HOME'] + \
        "/Library/Application Support/Syncthing/config.xml"

    try:
        config = open(config_file)
    except FileNotFoundError:
        return ['ERROR: config file not found: '+ config_file]

    xml_contents = config.read()
    config.close()

    PATH_REGEX = re.compile('path="(.*)" type')
    shares = PATH_REGEX.findall(xml_contents)

    c_list = []

    for share in shares:
        for root, dirs, files in os.walk(share):
            for file in files:

                if "/.stversions/" in root:
                    continue

                if ".sync-conflict-" in file:
                    c_list.append('--' + os.path.join(root, file) +
                                  ' | terminal=false bash=/usr/bin/open param1=\"' + root + '\"')

    if c_list:
        c_list = ['Conflicts'] + c_list

    return c_list


def main():
    """ Syncthing Bitbar plugin """

    parser = argparse.ArgumentParser(description='Bitbar - Syncthing conflicts helper.')
    parser.add_argument('conflict', nargs='?',
                        help='a conflict')
    args = parser.parse_args()

    print(args)

    conflicts = find_conflicts()

    if conflicts:
        print(u' \u001b[31mst\u001b[0m | ansi=true')
    else:
        print('st')

    print('---')

    if args.conflict:
        print('CONFLICT--', args.conflict)
        call(["open", args.conflict])

    for item in conflicts:
        print(item)

    return 0


if __name__ == '__main__':
    sys.exit(main())
