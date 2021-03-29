#!/usr/bin/env python3

# <xbar.title>Ifinit Drive Journal Stats</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Dimitrie Hoekstra</xbar.author>
# <xbar.author.github>strages</xbar.author.github>
# <xbar.desc>Short description of what your plugin does.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/strages/infinit-drive-journal-stats-bitbar-plugin/master/screenshot.png</xbar.image>
# <xbar.dependencies>python3</xbar.dependencies>
# <xbar.abouturl>https://github.com/strages/infinit-drive-journal-stats-bitbar-plugin</xbar.abouturl>


import json
import os
import subprocess

p = subprocess.Popen(
  ['infinit-journal', '--stat', '--script'],
  stdout = subprocess.PIPE,
  env = {'PATH': '/usr/local/bin:%s' % os.environ['PATH']},
)
out, err = p.communicate()
res = json.loads(out.decode('utf-8'))

total_size = 0
for k, v in res.items():
  total_size += v['size']

def GetHumanReadable(size,precision=0):
    suffixes=[' B',' KB',' MB',' GB',' TB']
    suffixIndex = 0
    while size > 1024 and suffixIndex < 4:
        suffixIndex += 1 #increment the index of the suffix
        size = size/1024.0 #apply the division
    return "%.*f%s"%(precision,size,suffixes[suffixIndex])

total_size_readable = GetHumanReadable(total_size)

print(total_size_readable)
