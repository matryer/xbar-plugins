#!/usr/bin/env PYTHONIOENCODING=UTF-8 /usr/local/bin/python
# <xbar.title>Text on Menubar</xbar.title>
# <xbar.version>1.0</xbar.version>
# <xbar.author>clip</xbar.author>
# <xbar.author.github>binderclip</xbar.author.github>
# <xbar.desc>Show your text on menubar</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.image>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</xbar.image>

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
