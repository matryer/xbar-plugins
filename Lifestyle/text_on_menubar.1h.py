#!/usr/bin/env PYTHONIOENCODING=UTF-8 /usr/local/bin/python
# <bitbar.title>Text on Menubar</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>clip</bitbar.author>
# <bitbar.author.github>binderclip</bitbar.author.github>
# <bitbar.desc>Show your text on menubar</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.image>https://qn.cdn.cliiip.com/imgs/u/d93e6b5b-52b7-4324-aaef-8969915f91b1.png</bitbar.image>

import argparse
import os
import subprocess


def get_text_file():
    home = os.path.expanduser("~")
    text_file = os.path.join(home, '.bitbar_text_on_menubar')
    return text_file


def get_file_path():
    return os.path.realpath(__file__)


def get_file_name():
    return os.path.basename(__file__)


def read_and_print():
    text_file = get_text_file()
    text = ''
    try:
        with open(text_file, 'r') as f:
            text = f.read()
    except IOError:
        pass
    print(text.strip() or 'Hello')


def set_text():
    try:
        ret = subprocess.check_output(
            [
                'osascript',
                '-e',
                r'set input_text to text returned of (display dialog "Please input text here:"'
                ' default answer "" with title "Set the Text")',
            ])
        text = ret.strip()

        text_file = get_text_file()
        with open(text_file, 'w') as f:
            f.write(text)
    except subprocess.CalledProcessError:
        pass
    # refresh
    s = "bitbar://refreshPlugin?name={}".format(get_file_name())
    subprocess.call(['open', s])


def print_submenu():
    print('---')
    print('Set the Text | bash="{}" param1="-s"  terminal=false'.format(get_file_path()))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-s", "--set_text",
                        action="store_true", help='set the text')
    args = parser.parse_args()

    if args.set_text:
        set_text()
        return
    read_and_print()
    print_submenu()


if __name__ == '__main__':
    main()
