#!/usr/bin/python

import os
import re
import sys

#
# <bitbar.title>Taskpaper Today</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Ernst Widerberg</bitbar.author>
# <bitbar.author.github>ernstwi</bitbar.author.github>
# <bitbar.desc>This plugin will display all tasks tagged @today and not @done in a given Taskpaper file.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/XODr2PY.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
#
# Read more about the Taskpaper format at taskpaper.com.
#
# For Vim users I recommend github.com/davidoc/taskpaper.vim. Also, to refresh
# the BitBar plugin on writes, add this to your .vimrc:
#
#     autocmd! BufWritePost ~/todo.taskpaper silent ! open -g
#                 \ "bitbar://refreshPlugin?name=taskpaper.*?.py"
#
# Set you own file location by changing the variable "taskpaper_file" below.
#

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

    # Stop searching if "Archive" header is reached.
    if re.match(r'archive:\n', line, re.IGNORECASE) != None:
        break

    # Update header (project title).
    if line.endswith(':\n'):
        header = line.strip()[:-1]

    # Include lines that contain @today and not @done in output. Remove leading
    # dash and "@today" with surrounding whitespace.
    elif ' @%s' % tag in line and ' @done' not in line:
        items += '%s (%s)\n' % (re.sub(' @%s ?' % tag, ' ', line).strip()[2:], header)
        num_items += 1

file.close()
sys.stdout.write('%d %s\n---\n%s'
        % (num_items, 'Task' if num_items == 1 else 'Tasks', items))
