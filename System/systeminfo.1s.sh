#!/usr/bin/env python3
#
#<xbar.title>System Info</xbar.title>
#<xbar.version>v1.1</xbar.version>
#<xbar.author>Jim Yu</xbar.author>
#<xbar.author.github>pafera</xbar.author.github>
#<xbar.desc>Displays CPU, memory, disk, network, and battery usage in a concise yet informative format</xbar.desc>
#<xbar.dependencies>python3, psutil</xbar.dependencies>
#<xbar.abouturl>https://pafera.com</xbar.abouturl>
#<xbar.image>https://pafera.com/systeminfo.screenshot.webp</xbar.image>
#
# Streamable didn't work for me, so I've adapted by caching disk and network bytes in JSON instead.

import json
import os
import psutil
import time

GRAPHCHARS  = '▁▂▃▄▅▆▇██'

def main():
  previousstats = {
    'diskreadbytes':    0,
    'diskwritebytes':   0,
    'netsentbytes':     0,
    'netreceivedbytes': 0,
  }

  try:
    with open(os.environ['HOME'] + '/systeminfo.json', 'r') as f:
      previousstats  = json.load(f)
  except Exception as e:
    print(e)

  cpupercent  = 100 - psutil.cpu_times_percent()[3]
  mempercent  = psutil.virtual_memory()[2]

  diskio    = psutil.disk_io_counters()

  diskreadbytes   = diskio[2] - previousstats['diskreadbytes']
  diskwritebytes  = diskio[3] - previousstats['diskwritebytes']

  previousstats['diskreadbytes']  = diskio[2]
  previousstats['diskwritebytes'] = diskio[3]

  netio     = psutil.net_io_counters()

  netsentbytes      = netio[0] - previousstats['netsentbytes']
  netreceivedbytes  = netio[1] - previousstats['netreceivedbytes']

  previousstats['netsentbytes']     = netio[0] 
  previousstats['netreceivedbytes'] = netio[1]
  
  with open(os.environ['HOME'] + '/systeminfo.json', 'w') as f:
    json.dump(previousstats, f)
  
  cpugraph  = GRAPHCHARS[round(cpupercent / 12.5)]
  memgraph  = GRAPHCHARS[round(mempercent / 12.5)]

  diskreadgraph   = f''' R {round(diskreadbytes / 1024)}'''
  diskwritegraph  = f''' W {round(diskwritebytes / 1024)}'''

  netsentgraph      = f''' TX {round(netsentbytes / 1024)}'''
  netreceivedgraph  = f''' RX {round(netreceivedbytes / 1024)}'''

  batteryinfo   = psutil.sensors_battery()

  timeleft      = '⚡'

  if batteryinfo[1] != -2:
    if batteryinfo[1] == -1:
      timeleft = '?'
    else:
      timeleft = f'''{round(batteryinfo[1] / 3600)}:{round((batteryinfo[1] % 3600) / 60)}'''

  batterygraph      = f''' {round(batteryinfo[0])}% {timeleft}'''

  print(f'''{cpugraph}{memgraph}{diskreadgraph}{diskwritegraph}{netsentgraph}{netreceivedgraph}{batterygraph}
---
CPU:\t\t\t\t{cpupercent}%
Memory:\t\t\t\t{mempercent}%
Disk Read:\t\t\t{diskreadbytes / 1024 / 1024:.2f} MiB
Disk Write:\t\t\t{diskwritebytes / 1024 / 1024:.2f} MiB
Network Sent:\t\t{netsentbytes / 1024 / 1024:.2f} MiB
Network Received:\t{netreceivedbytes / 1024 / 1024:.2f} MiB
''')

main()
