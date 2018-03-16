#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Quod Libet Control</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Sean Lane</bitbar.author>
# <bitbar.author.github>seanlane</bitbar.author.github>
# <bitbar.desc>Control Quoid Libet via Bitbar</bitbar.desc>
# <bitbar.image>https://i.imgur.com/VA8ZEbR.png</bitbar.image>
# <bitbar.dependencies>quodlibet, python</bitbar.dependencies>

import os, shutil, subprocess, sys

CUR_PATH = os.path.expanduser('~/.quodlibet/current')
CONTROL_PATH = os.path.expanduser('~/.quodlibet/control')

if not os.path.exists(CUR_PATH):
  print("")
  sys.exit()

def get_current():
  with open(CUR_PATH, 'r') as cur:
    lines = cur.readlines()
  lines = [x.strip().lstrip('~').lstrip('#').split('=', 1) 
    for x in lines]
  return {x[0]: x[1] for x in lines}

def run_cmd(cmd_in):
  if cmd_in == 'delete':
    status = get_current()
    run_cmd('next')
    if os.path.exists(status['filename']):
      subprocess.check_output('mv "{}" {}'.format(
        status['filename'], os.path.expanduser('~/.Trash/')), shell=True)
  elif cmd_in == 'menu-previous':
      run_cmd('previous')
      run_cmd('previous')
  else:
    subprocess.check_output('echo {} > {}'.format(cmd_in, CONTROL_PATH), shell=True)

def cmd(phrase, param1):
  return '{} | bash={} param1={} terminal=false'.format(
    phrase, sys.argv[0], param1) 

if len(sys.argv) == 1:
  status = get_current()

  print('♫ {} - {} | length=30'.format(
    status['title'], status['artist']))
  print('---')
  print(status['title'])
  print(status['album'])
  print(status['artist'])
  print(cmd('Play / Pause', 'play-pause'))
  print(cmd('Next', 'next'))
  print(cmd('Previous', 'menu-previous'))
  print('---')
  print(cmd('Delete', 'delete'))
elif len(sys.argv) > 1:
    run_cmd(sys.argv[1])
