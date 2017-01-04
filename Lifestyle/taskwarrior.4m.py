#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Taskwarrior
#
# <bitbar.title>Taskwarrior</bitbar.title>
# <bitbar.version>v2.0</bitbar.version>
# <bitbar.author>Christoph Russ</bitbar.author>
# <bitbar.author.github>christophruss</bitbar.author.github>
# <bitbar.desc>Task managment through your menu-bar.</bitbar.desc>
# <bitbar.dependencies>task,python</bitbar.dependencies>
# <bitbar.image>http://i.imgur.com/vjEt7Is.jpg</bitbar.image>
#
# Dependencies:
#   taskwarrior (http://taskwarrior.org)
#      available via homebrew `brew install task`

#
# TODO
# * In a sub-menu (at the bottom) Allow to restart previously completed tasks
# * Allow to delete tasks (also in a sub-menu?)
#

import os
import subprocess
from subprocess import Popen, PIPE


def trigger_actions(argv):
    t_id = argv[-1]
    action = argv[-2]
    subprocess.call(['/usr/local/bin/task', t_id, action])


def build_command(t_id, action, refresh=True):
    cmd = ''

    if refresh:
        cmd = cmd + ' refresh=true'

    cmd = cmd + ' bash=/usr/bin/python param1=' + __file__ + ' param2='

    cmd = cmd + action

    cmd = cmd + ' param3=' + str(t_id)

    cmd = cmd + ' terminal=false'

    return cmd


def print_output(
        cmd,
        color,
        head,
        print_content=True,
        command='',
        ignore_id_list=[],
        highlight_id_list=[],
        highlight_color=' color=Red',
        highlight_command='',
        alternate_command=''):
    output = ''

    # important: PIPE the stderr, since task likes to use that - a lot ...
    p = Popen(['/usr/local/bin/task', cmd],
              stdin=None, stdout=PIPE, stderr=PIPE)
    output, err = p.communicate()

    output_lines = output.split('\n')

    id_list = []
    content_lines = []

    for output_line in output_lines:
        output_line = output_line.strip()
        if not output_line:
            continue
        # When looking for 'active' or 'next' tasks, we want to look only at
        # lines that start with a digit or a -. Other line are likely extra
        # data like annotations from bugwarrior sync
        content_id = output_line.split()[0]
        if not content_id.isdigit() and content_id not in ['--', '-', 'ID']:
            continue
        content_lines.append(output_line)

    content_count = len(content_lines[2:-1])

    if head:
        if content_count == 0:
            print 'ⓣ'  # ⓪ #⓿
        elif content_count < 21:
            circle_number = ['⓪', '①', '②', '③', '④', '⑤', '⑥',
                             '⑦', '⑧', '⑨', '⑩', '⑪', '⑫', '⑬',
                             '⑭', '⑮', '⑯', '⑰', '⑱', '⑲', '⑳']
            print circle_number[content_count]  # + '|' + color_pending
        else:
            print str(content_count)  # + '| color=Red'

        print '---'

    if content_count < 1:
        return id_list

    table_head = content_lines[0]

    # total_number_of_tasks = content_lines[-1]

    content_formatting = ' | size=12 font=Courier'

    if print_content:
        print table_head + content_formatting
        print '---'

    for content_line in content_lines[2:-1]:
        content_id = content_line.split()[0]

        if content_id == '-':
            # should be the UUID in this case
            content_id = content_line.split()[1]

        if content_id in ignore_id_list:
            continue

        id_list.append(content_id)

        if print_content:
            cmd = ''

            if len(command) > 0:
                cmd = build_command(t_id=content_id, action=command)

            if content_id in highlight_id_list:
                if len(highlight_command) > 0:
                    cmd = build_command(
                        t_id=content_id, action=highlight_command)
                print content_line + content_formatting + highlight_color + cmd
            else:
                print content_line + content_formatting + color + cmd

            # adding an alternative command (press ALT for this!)
            # printing the same stuff again, only with a different action
            # attached
            if len(alternate_command) > 0:
                alt_cmd = build_command(
                    t_id=content_id, action=alternate_command)

                if content_id in highlight_id_list:
                    print content_line + content_formatting + highlight_color + alt_cmd + ' alternate=true'
                else:
                    print content_line + content_formatting + color + alt_cmd + ' alternate=true'

    return id_list


def is_darkmode():
    FNULL = open(os.devnull, 'w')
    return_code = subprocess.call(['/usr/bin/defaults', 'read', '-g',
                                   'AppleInterfaceStyle'], stdout=FNULL, stderr=subprocess.STDOUT)
    if (return_code == 1):
        return False
    else:
        return True


def main(argv):

    if len(argv) > 1:
        trigger_actions(argv)
        exit()

    if is_darkmode():
        color_running = ' color=Red'
        color_pending = ' color=Yellow'
        color_completed = ' color=Green'
    else:
        color_running = ' color=Red'
        color_pending = ' color=Black'
        color_completed = ' color=Green'

    id_list = print_output('active', color_running, True, print_content=False)

    if len(id_list) > 0:
        print '---'

    print_output(
        'next',
        color_pending,
        False,
        command='start',
        highlight_id_list=id_list,
        highlight_color=color_running,
        highlight_command='stop',
        alternate_command='done')

    print '---'

    # ok, so if you want to delete a command, you have to press done first ...
    # sorry, but there is only one alternative command I can provide above ...
    print_output(
        'completed',
        color_completed,
        False,
        command='start',
        alternate_command='delete')

    return

if __name__ == "__main__":
    import sys
    main(sys.argv)
