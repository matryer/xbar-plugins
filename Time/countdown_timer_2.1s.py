#!/usr/bin/env python
# -*- coding: UTF-8 -*-
# <bitbar.title>Countdown Timer 2</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Federico Ferri</bitbar.author>
# <bitbar.author.github>fferri</bitbar.author.github>
# <bitbar.desc>Simple countdown timer.</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.image>https://raw.githubusercontent.com/fferri/bitbar-countdown-timer/master/screenshot.gif</bitbar.image>
# <bitbar.abouturl>https://github.com/fferri/bitbar-countdown-timer</bitbar.abouturl>

import os
import re
import subprocess
import sys
import time

icon='iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAxZJREFUWAntlluITVEYx8+4jbuQ++CFSC655Tau8aA8eFKaJzKvlHfFwzwg1+TVRJNQU5SSUAZNIyNyy4gQcikkJLn9/lnLrHXss9bee868zVe/sy7f9//WOmvvvdYqFLosvAIVYXfQW4n3JPRLiNpGX3NCf6d29Sf77xKszztyt7zCiK6z8kaGLRTWEvEY3JXaTXtoVJkzYD66IQGt3qM74E6oJhDfIddkM1ArpTupWbTr4IXxu5Ox9a/46mEZlMWqyPIT7ADDqWs16p0++b7DXTgNjXAV3oPVqWyBBZDbqlG6CTWxBnji9B+lvhAGQbH1pGMCbIa3YHPtpJ7LZqJqA5vILa/QvyhD1t7E7nJy7cmg9UL1eFbDWbATOuhFZGusJFyPV7nqskn9aL0fSnLI787Vsh+J8ulxZ7bzKCR+kFnpC3o5zVrqyvnR6UtVHWeEEk9Lpfg/SF/q8wS93kPl1SuR2vYRKdGR1Ao/cKTR64DVV+ea/Yq1oaYy3QA+gya0JpXCD+pjtLoJlLI3OJR/SqkAt3+eCf5BOdh1BOrd8ekaMhZ+QQOETJ+/JqS9yrOkU3muibhN+cGLTm4sN3GvKe/BMYidZ03EyPQHPEua0GgToZ05ZtJfghOgk78vHICYvTQBA4sDkyZkX0I9sphZvVblogleERPh13Eks2P9bfHb41+tvfLJVIe1d5WsadLb4QK8gsuwH2Jmbw+6FXiWNKFHJmKGF1m6sQNXC0yCNI9LmabqB9PNIGpjiNAXIGZHo/MFXDP5U9+9bxnB1nzjBVVVJrf+8IBgpONcZ0TPnL5yVfea3IezJrSPrSPXjuIxx5vJKPeoYmesXeuIs1zKQnm1t2kyx0NBId85k0DHwZJQYMSnr1kbqCbzFHKbDsqHoERiI2S16QhugPTfQO0OWSXq+2An1UR9aYqM2j7sISqtbhC6NQatIuj1ndqBtzhd2kCvQyu0gXZtnYNagWqYA9bOUNkE72xHucpVJGoEu1qxUkfJhiyDZ1khN+9EGtplF8MI0AanXF9Aq3ATTkEzdFlZV+APQ77IUZhTv+IAAAAASUVORK5CYII='

def prompt(text='', defaultAnswer='', icon='note', buttons=('Cancel','Ok'), defaultButton=1):
    try:
        d = locals()
        d['buttonsStr'] = ', '.join('"%s"' % button for button in buttons)
        d['defaultButtonStr'] = isinstance(defaultButton, int) and buttons[defaultButton] or defaultButton
        return subprocess.check_output(['osascript', '-l', 'JavaScript', '-e', '''
            const app = Application.currentApplication()
            app.includeStandardAdditions = true
            const response = app.displayDialog("{text}", {{
                defaultAnswer: "{defaultAnswer}",
                withIcon: "{icon}",
                buttons: [{buttonsStr}],
                defaultButton: "{defaultButtonStr}"
            }})
            response.textReturned
        '''.format(**d)]).rstrip()
    except subprocess.CalledProcessError:
        pass

def notify(text, title, sound='Glass'):
    os.system('osascript -e \'display notification "{}" with title "{}" sound name "{}"\''.format(text, title, sound))

def entry(title='---', **kwargs):
    args = ' '.join('{}=\'{}\''.format(k,v) for k,v in kwargs.items() if v is not None)
    if args: args = '|' + args
    print(title + args)

def parse_time(s):
    m = re.match('^((\d+)h)?((\d+)m)?((\d+)s?)?$', s)
    if m is None: raise Exception('invalid time: %s' % s)
    h, m, s = map(int, (m.group(i) or 0 for i in (2, 4, 6)))
    return s + 60 * (m + 60 * h)

def render_time(t):
    t = int(round(t))
    h = t // 3600
    t -= h * 3600
    m = t // 60
    t -= m * 60
    k, v = 'hms', (h, m, t)
    return ''.join('%d%s' % (v[i], k[i]) for i in range(3) if i == 2 or any(v[:i+1]))

def read_data_file(filename):
    with open(data_file, 'rt') as f:
        lines = f.readlines()
    t = float(lines[0])
    task = lines[1].rstrip() if len(lines) > 1 else None
    return t, task

def write_data_file(filename, t, task=None):
    with open(data_file, 'wt') as f:
        f.write('{:f}{}{}'.format(t, '\n' if task else '', task or ''))

def usage():
    print('''usage: {0} <time> [task_name]
time can be:
    N or Ns: number of seconds
    Nm: number of minutes
    Nh: number of hours

example:
    {0} 5m30s 'Egg is ready!'
'''.format(__file__))
    sys.exit(1)

data_file = os.path.join(os.path.dirname(os.path.realpath(__file__)), '.' + os.path.basename(__file__) + '.countdown')

if len(sys.argv) == 1:
    if os.path.isfile(data_file):
        t, task = read_data_file(data_file)
        remain = int(round(max(0, t - time.time())))
        if remain == 0:
            notify('Times up!', task or 'Times up!')
            os.remove(data_file)
        title = '{}{}{}'.format(task or '', task and ': ' or '', render_time(remain))
        entry(title, color=('red' if remain <= 10 else 'orange' if remain < 60 else None))
    else:
        entry('|templateImage=\'%s\'' % icon)
    entry('---')
    if os.path.isfile(data_file):
        entry('Cancel timer', bash=__file__, param1='cancel', terminal='false')
    else:
        entry('Set timer...', bash=__file__, param1='set', terminal='false')
elif len(sys.argv) == 2 and sys.argv[1] == 'set':
    timestr = prompt('Input time (example: 30s, 15m, 1h, 1m30s)', '5m', 'note', ('Cancel','Set'), 1)
    task = prompt('Input task name')
    t = time.time() + parse_time(timestr)
    write_data_file(data_file, t, task)
elif len(sys.argv) == 2 and sys.argv[1] == 'cancel':
    os.remove(data_file)
