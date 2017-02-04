#!/usr/bin/python

#
# <bitbar.title>Taskpaper Today</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Ernst Widerberg</bitbar.author>
# <bitbar.author.github>ernstwi</bitbar.author.github>
# <bitbar.desc>Display Taskpaper items tagged "today" in the menu bar.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/XODr2PY.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
#
#
# Set you own file location by changing the variable "taskpaper_file" below.
#

import os
import re
import sys

taskpaper_file = '~/todo.taskpaper'
tag = 'today'

try:
    file = open(os.path.expanduser(taskpaper_file), 'r')
except IOError:
    sys.stdout.write('Taskpaper File Not Found\n---\n'
                     + 'Currently selected file is %s\n' % taskpaper_file
                     + 'To select a different file, edit %s|href=file://%s\n'
                     % (sys.argv[0], sys.argv[0]))
    sys.exit(1)

header = ''
items = ''
num_items = 0
for line in file:
    if re.match(r'archive:\n', line, re.IGNORECASE) != None:
        break
    if line.endswith(':\n'):
        header = line.strip()[:-1]
    elif line.endswith('@%s\n' % tag):
        items += '%s(%s)\n' % (line.partition('@' + tag)[0][2:], header)
        num_items += 1

file.close()
sys.stdout.write('%d %s\n---\n%s'
        % (num_items, 'Task' if num_items == 1 else 'Tasks', items))
