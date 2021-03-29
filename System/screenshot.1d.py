#!/usr/bin/env python
# coding=utf-8
# <xbar.title>Screenshot</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.author>Brandon Barker, Soumya Ranjan Mohanty</xbar.author>
# <xbar.author.github>ProjectBarks, geekysrm</xbar.author.github>
# <xbar.desc>Allows for screenshots to be uploaded, saved, and added to the clipboard</xbar.desc>
# <xbar.image>http://i.imgur.com/51rg3EJ.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://github.com/matryer/bitbar-plugins/blob/master/System/screenshot.1d.py</xbar.abouturl>

import os, subprocess, tempfile, hashlib, requests, sys, platform, time
from distutils.version import StrictVersion

SAVE_PATH = "~/Pictures/"


def screenshot(path, copy_to_clipboard=False, show_cursor=False, show_errors=False, interactive=False,
               only_main_monitor=False, window_mode=False, open_in_preview=False, selection_mode=True,
               sounds=True, delay=5):
    params = ""
    if copy_to_clipboard:
        params += "-c "
    if show_cursor:
        params += "-C "
    if show_errors:
        params += "-d "
    if interactive:
        params += "-i "
    if only_main_monitor:
        params += "-m "
    if window_mode:
        params += "-o "
    if open_in_preview:
        params += "-P "
    if selection_mode:
        params += "-s "
    if sounds:
        params += "-x "
    if delay != 5 and delay >= 0:
        params += "-T {} ".format(delay)

    os.system("screencapture {} {}".format(params, path))
    return os.path.isfile(path)


def text_to_clipboard(output):
    process = subprocess.Popen('pbcopy', env={'LANG': 'en_US.UTF-8'}, stdin=subprocess.PIPE)
    process.communicate(output.encode('utf-8'))


def notify(title, subtitle, message):
    command = "display notification \"{}\" with title \"{}\"".format(message, title)
    if len(subtitle) > 0:
        command += " subtitle \"{}\"".format(subtitle)
    os.system("osascript -e '{}'".format(command))


def upload_image(upload):
    # upload is the path of the image
    headers = {
        'authorization': 'Client-ID 6fcd294cd0e8aa1',
    }  
    files = {
        'image': (open(upload, 'rb')),
    }
    r = requests.post('https://api.imgur.com/3/upload', headers=headers, files=files)
    import json
    return json.loads(r.text)['data']['link']

class Command(object):
    def __init__(self, title, name):
        self.title = title
        self.name = name

    def get_name(self):
        return self.name

    def get_description(self):
        return "{0} |bash={2} param1={1} terminal=false".format(self.title, self.name, os.path.realpath(__file__))

    def execute(self):
        raise Exception("Abstract Function")


class Upload(Command):
    def __init__(self):
        super(Upload, self).__init__("Upload Online", "upload")

    def execute(self):
        temp_path = tempfile.NamedTemporaryFile().name + ".png"
        if not screenshot(temp_path):
            exit()
        notify("Uploading Screenshot", "", "Your image is being uploaded online!")
        url = upload_image(temp_path)
        os.remove(temp_path)
        notify("Copied Screenshot", "", "Image URL copied to clipboard!")
        text_to_clipboard(url)


class Clipboard(Command):
    def __init__(self):
        super(Clipboard, self).__init__("Copy to Clipboard", "clipboard")

    def execute(self):
        temp_path = tempfile.NamedTemporaryFile().name + ".png"
        if not screenshot(temp_path):
            exit()
        os.system("osascript -e 'set the clipboard to POSIX file \"{}\"'".format(temp_path))
        notify("Copied Screenshot", "", "Image copied to clipboard!")


class SaveFile(Command):
    def __init__(self):
        super(SaveFile, self).__init__("Save to File", "save")

    def execute(self):
        temp_path = os.path.join(os.path.expanduser(SAVE_PATH), time.strftime("screenshot-%Y%m%d-%H%M%S.png"))
        parent = os.path.dirname(temp_path)
        if not os.path.isdir(parent):
            os.mkdir(parent)
        if not screenshot(temp_path):
            exit()
        os.system("open -R {0}".format(temp_path))


version = platform.mac_ver()[0]
if StrictVersion(version) < StrictVersion("10.9"):
    raise Exception("Mac OSX is too old!")

sub_commands = [Upload(), Clipboard(), SaveFile()]

if len(sys.argv) <= 1:
    print("📸")
    print("---")
    for sub_command in sub_commands:
        print(sub_command.get_description())
else:
    try:
        for sub_command in sub_commands:
            if sub_command.get_name() != sys.argv[1]:
                continue
            sub_command.execute()
    except Exception, e:
        notify("Error", "", str(e))
