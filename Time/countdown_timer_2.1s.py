#!/usr/bin/env python
# -*- coding: UTF-8 -*-
# <bitbar.title>Countdown Timer 2</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Federico Ferri</bitbar.author>
# <bitbar.author.github>fferri</bitbar.author.github>
# <bitbar.desc>Simple countdown timer.</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.image>https://lh6.googleusercontent.com/4WZ6xg0oobO5EiRtv9NNokifv5ED8dFX-hWfcD9onoy5xxyFW6dfZDD5xYdX_qFxbI0D24Ot3MjMK4UJxx0s=w2482-h1366</bitbar.image>

import os
import re
import subprocess
import sys
import time

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
        entry('⏳')
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
