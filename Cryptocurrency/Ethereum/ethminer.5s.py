#!/usr/local/bin/python3

# <bitbar.title>EthMiner</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Cédric Foellmi</bitbar.author>
# <bitbar.author.github>onekiloparsec</bitbar.author.github>
# <bitbar.desc>Start and stop the ethminer.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/DeWnKKf.png</bitbar.image>
# <bitbar.dependencies>python3,psutil,ethminer</bitbar.dependencies>

##############################################################################
### SETUP ###
# 0. Install the ethminer executable.
#    For instance from https://github.com/ArtSabintsev/Ethminer-for-macOS
#
# 1. Create a shell script with the full ethminer command, using the executable above.
#    For instance 'ethmining.sh', with the ethminer executable inside "~/bin":
#    Note also the last part '>& /tmp/ethmining.log':
#
# #!/bin/bash
# /Users/<YourName>/bin/ethminer -G -t 2 -v 2 -S <PoolAddress> -u <UserName> >& /tmp/ethmining.log
#
# 2. Make the script executable: chmod 755 ethmining.sh
#
# 3. Fill the MINING_SCRIPT_FULL_PATH variable below. It is important to provide
# the full path of the script. No ~, no ./... etc.
#
# 4. Make sure MINING_LOG_FULL_PATH points to the same file as the last part of the script above.
#
# 5. Consider a small tip, thanks! :-) ETH: 0x1Dd0e805a5DC21e40B28D4763c6Af790cd6bcd33
##############################################################################

import os

##############################################################################
# Customize your variables here
MINING_SCRIPT_NAME = "ethmining.sh"
MINING_SCRIPT_FULL_PATH = "{}/bin/{}".format(os.environ['HOME'], MINING_SCRIPT_NAME)
MINING_LOG_FULL_PATH = "/tmp/ethmining.log"
MINING_LOG_LINES_NUMBER = "10" # MUST BE LARGER THAN 0
##############################################################################

import re
import psutil
import subprocess

started = 'ethminer' in [ p.name() for p in psutil.process_iter(attrs=['name']) ]
ansi_escape = re.compile(r'\x1B\[[0-?]*[ -/]*[@-~]')

print("| templateImage=iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAAlwSFlzAAALEwAACxMBAJqcGAAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAoRJREFUOBF9Uk2IUlEUfvf9+JMS5K7nyORALRqwYdyW4+CiWUSLSNsEUQPKgERTIRQ2WplTMs2ihWDtpJUStRNy43YCZ5EIFUQOikoIo4HO0/fXOa+eORkdeLx7zz3fd8/3nUtR/4h4PE5jOhKJzITDYSeu9RyuJ4Od3OjrUqmEBArLGV/UG1+OwXq5Wq0S/fy/f7hJI02ltpcePU6oS95l1eVyXUSQ2+3m/gZrrU4kCRBIuBclYZvQNCXLsCVkK5PJcOVyWYSjQ50cIoAi7fZEMnmLMxgXxdFIIoSIqqqezGazUSSGLg7JHhP4/X4mFAqJ0AGvyMpDRZYphmHwnFUUhToQhHs+n+8UdoG1SIYxJvi1pSiLxfrUbDYf7XQ6YqVSoev1OgEClMX1+/0trMvn86perzGhcel0Wo5Go5f7g0Fid7c8go9tt5vkR69H9bpdagRBM8zpE3NzzWajUfZ6vWytVlN0PQoyQqK382HnI3ThMplMlMFgkPb3u+AhYQVBMLZbrc8Oh+M71sKoNYwmIRaLaS15PJ691Rur9wf9flgUxR54wILlLE3TB1ar9e78/Pxtp9P5FQn00EeCfyQhT5Kb7ziWa+7tfXv1vli8CZMw8Tz/HHy4AtM4Y7PZLhQKhSHWIkY3Uc3lcuiHesRsuiYMhdBx3l4Mra0VZ2Zn38AE3oqSdAfA1xGM+rEWvj9TCAQCMj6W9fX1LmPgfOCBbSQMXzOE5GRFtlstlksAboAMA+jXHhsSTEUwGNSe6+bmsxTIUc+e86gLiwsvfxfqpk/hxgnQqftCPdjY+HR+ZaUFhxoQxq1LHtdPJWBkuh/UUBCu2nneD9US6gYCbXRjNCx+ArWREQKYptF1AAAAAElFTkSuQmCC")
print("---")

if started is True:
    print("Stop ethminer | terminal=false bash=/usr/bin/killall param1=ethminer")
else:
    print("Start ethminer | terminal=false bash=\"{}\"".format(MINING_SCRIPT_FULL_PATH))

print("---")

if started is True:
    print("Running !")

proc = subprocess.run(['tail', '-'+MINING_LOG_LINES_NUMBER, MINING_LOG_FULL_PATH], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
if proc.returncode == 0:
    if not started:
        print("Last log lines of the previous session:")

    for line in proc.stdout.split(b'\n'):
        s = line.replace(b'|', b' ').replace(b'\xe2\x84\xb9', b' ')
        print(s.decode('utf8'))
