#!/usr/bin/env python3

# <xbar.title>Recent Apps</xbar.title>
# <xbar.version>v0.1</xbar.version>
# <xbar.author>Justin Miller</xbar.author>
# <xbar.author.github>incanus</xbar.author.github>
# <xbar.desc>Show & launch recently used apps not permanently in the Dock</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/incanus/xbar-recent-apps/main/screenshot.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://github.com/incanus/xbar-recent-apps</xbar.abouturl>

from datetime import datetime
from os import getenv
from subprocess import check_output, run

DB = getenv('HOME') + '/Library/Caches/recent-apps'

apps = {}
new = True

now = datetime.now().isoformat()

run('touch ' + DB, shell=True)

with open(DB, 'r+') as f:
    entries = f.read().strip().split("\n")
    f.seek(0)
    for entry in entries:
        try:
            (name, id, date) = entry.split('|')
            apps[name] = { 'id': id, 'date': date }
        except ValueError:
            pass
    for line in check_output(
    """
    /usr/bin/defaults read com.apple.dock recent-apps | \
    grep "bundle-identifier\|file-label" | \
    awk -F ' = ' '{ print $2 }' | \
    sed -e 's/;//'
    """, shell=True).decode('utf-8').strip().split("\n"):
        if new:
            id = line.strip('"')
            new = False
        else:
            name = line.strip('"')
            if not name in apps.keys():
                apps[name] = { 'id': id, 'date': now }
            new = True
    apps = sorted(apps.items(), key=lambda app: app[1]['date'], reverse=True)
    for app in apps:
        f.write(app[0].strip('"') +'|' + app[1]['id'].strip('"') + '|' + app[1]['date'] +"\n")
    print(':clock: | shortcut=CMD+OPTION+A')
    print('---')
    f.seek(0)
    entries = f.read().strip().split("\n")
    for entry in entries[:10]:
        (name, id, date) = entry.strip().split('|')
        print(f"{name} | bash=/usr/bin/open terminal=false param0=-b param1={id}")