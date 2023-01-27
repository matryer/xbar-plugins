#!/usr/bin/env python3

# <xbar.title>Show high CPU usage process</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Oleg Kalachev</xbar.author>
# <xbar.author.github>okalachev</xbar.author.github>
# <xbar.desc>Show the top process if it's using more than 50% of CPU. Show 5 top processes in the submenu.</xbar.desc>
# <xbar.image>https://gist.github.com/okalachev/cfa62f3acc49eb14059f060dc3259a80/raw/f90f4e1034414240ca1fc8a562ecfbb1cbddfa3b/cpu.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://chev.me/cpu-high-usage/</xbar.abouturl>

# <xbar.var>number(THRESHOLD=50): CPU usage threshold to show in the menu bar.</xbar.var>
# <xbar.var>number(NUMBER=5): Number of processes to show in the submenu.</xbar.var>
# <xbar.var>boolean(KILL=false): Kill the process when clicked in the submenu.</xbar.var>
# <xbar.var>number(LENGTH=20): Maximum length of string in the manu bar.</xbar.var>
# <xbar.var>string(TEXT=cpu): Default menu bar text.</xbar.var>

import os
import subprocess

THRESHOLD = float(os.environ.get('THRESHOLD', 50))
NUMBER = int(os.environ.get('NUMBER', 5))
KILL = os.environ.get('KILL', 'false') != 'false'
LENGTH = int(os.environ.get('LENGTH', 5))
TEXT = os.environ.get('TEXT', 'cpu')

ps = subprocess.check_output('ps -cAr -o pid,pcpu,command'.split(' '))

for i, p in enumerate(ps.splitlines()[1:NUMBER + 1]):
	s = p.strip().split()
	pid = int(s[0])
	cpu = float(s[1])
	command = b' '.join(s[2:]).decode()
	if i == 0:
		if cpu > THRESHOLD:
			print('{}% {} | length={}'.format(round(cpu), command, LENGTH))
		else:
			print(TEXT)
		print('---')
	shell = 'shell=kill param1=-9 param2={}'.format(pid) if KILL else ''
	print('{}% {} ({}) | {}'.format(cpu, command, pid, shell))
