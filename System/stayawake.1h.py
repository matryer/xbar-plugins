#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Stay Awake</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Christoph Russ</bitbar.author>
# <bitbar.author.github>christophruss</bitbar.author.github>
# <bitbar.desc>Keep your computer awake with a single click.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/pkqXw8x.jpg</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.abouturl></bitbar.abouturl>

import sys
import subprocess

def run_detached(cmd):
    subprocess.Popen(cmd, shell=False,
        stdin=None, stdout=None, stderr=None, close_fds=True)

if len(sys.argv) > 1:
    if 'start' in sys.argv:
        run_detached(['/usr/bin/caffeinate','-ids'])
    exit()

stay_awake = False

caffeinate_running = subprocess.check_output('/bin/ps aux', shell=True).strip().split('\n') # | grep caffeinate

for ps_line in caffeinate_running:
    if 'caffeinate' in ps_line:
        stay_awake = True

#'font=Arial' # 'size=12' 'size=14' 'size=9' # ☕ # ⚡ # 💡 # 🚨  m,.
# :coffee: # :tea: # :sparkles: # :dizzy: # :zzz: # :runner: :running: # :eyes: # :bulb: #:sunny: #:cloud: # :zap:
# :low_brightness: :high_brightness: :lock: # :unlock: # :rotating_light:

if stay_awake:
    print '☕ | size=14 refresh=true bash=/usr/bin/killall param1=caffeinate terminal=false'
else:
    print '☕ | size=9 refresh=true bash=/usr/bin/python param1='+__file__+' param2=start terminal=false'
