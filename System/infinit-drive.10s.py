#!/usr/bin/env python3

# <bitbar.title>Ifinit Drive Journal Stats</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Dimitrie Hoekstra</bitbar.author>
# <bitbar.author.github>strages</bitbar.author.github>
# <bitbar.desc>Short description of what your plugin does.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/strages/infinit-drive-journal-stats-bitbar-plugin/master/screenshot.png</bitbar.image>
# <bitbar.dependencies>python3</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/strages/infinit-drive-journal-stats-bitbar-plugin</bitbar.abouturl>


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
