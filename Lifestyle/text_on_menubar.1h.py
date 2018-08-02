#!/usr/bin/env PYTHONIOENCODING=UTF-8 /usr/local/bin/python3
# <bitbar.title>Text on Menubar</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>clip</bitbar.author>
# <bitbar.author.github>binderclip</bitbar.author.github>
# <bitbar.desc>Show your text on menubar</bitbar.desc>
# <bitbar.dependencies>python3</bitbar.dependencies>
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
        with open(text_file, 'r', encoding='UTF-8') as f:
            text = f.read()
    except FileNotFoundError:
        pass
    print(text.strip() or 'Hello')


def set_text():
    ret = subprocess.run(
        [
            'osascript',
            '-e',
            r'set input_text to text returned of (display dialog "Please input text here:"'
            ' default answer "" with title "Set the Text")',
        ], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    text = ret.stdout.strip().decode('utf-8')

    text_file = get_text_file()
    with open(text_file, 'w', encoding='utf-8') as f:
        f.write(text)
    # refresh
    s = "bitbar://refreshPlugin?name={}".format(get_file_name())
    subprocess.run(['open', s])


def print_submenu():
    print('---')
    print('Set the Text | bash="{}" param1="-s"  terminal=false'.format(get_file_path()))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-s", "--set_text", action="store_true", help='set the text')
    args = parser.parse_args()

    if args.set_text:
        set_text()
        return
    read_and_print()
    print_submenu()


if __name__ == '__main__':
    main()
