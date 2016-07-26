#!/usr/bin/env python2
#- * -coding: utf - 8 - * -
#
# <bitbar.title>Battery Health</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Andros Fenollosa</bitbar.author>
# <bitbar.author.github>tanrax</bitbar.author.github>
# <bitbar.desc>Shows power percentaje and notice when you load</bitbar.desc>

import os, math, subprocess, pickle, tempfile

# Get info battery
SAVE_LOCATION = os.path.join(tempfile.gettempdir(), 'batteryHealth2.pkl')
LIM_LOWER = 45
LIM_UPPER = 85
p = subprocess.Popen(["ioreg", "-rc", "AppleSmartBattery"], stdout = subprocess.PIPE)
output = p.communicate()[0]
b_max = 0
o_cur = 0
is_charging = ''
dateSave = False
alertMin = False
alertMax = False

try:
    dateFile = open(SAVE_LOCATION)
    dateSave = pickle.load(dateFile)
    alertMin = dateSave['alertMin']
    alertMax = dateSave['alertMax']
except:
    pass

# Get variables
for l in output.splitlines():
    if 'MaxCapacity' in l:
        o_max = l
    if 'CurrentCapacity' in l:
        o_cur = l
    if 'IsCharging' in l:
        is_charging = l

b_max = float(o_max.rpartition('=')[-1].strip())
b_cur = float(o_cur.rpartition('=')[-1].strip())
is_charging = str(is_charging.rpartition('=')[-1].strip())
if is_charging == 'Yes':
    is_charging = True
else:
    is_charging = False

# Calculate porcent battery
charge = b_cur / b_max
charge_porcent = int(math.ceil(100 * charge))

# Logic
## Alert Min
if LIM_LOWER >= charge_porcent and not alertMin:
    alertMin = True
    os.system('osascript -e \'display notification "Battery too low" with title "Battery health" sound name "Blow"\'')

## Alert Max
if LIM_UPPER <= charge_porcent and not alertMax:
    alertMax = True
    os.system('osascript -e \'display notification "Very charged battery" with title "Battery health" sound name "Blow"\'')

## Reset alerts
if LIM_UPPER > charge_porcent > LIM_LOWER:
    alertMin = False
    alertMax = False
    
# Save
dateTemp = {'alertMax': alertMax, 'alertMin': alertMin}
dateSave = open(SAVE_LOCATION, 'w+')
pickle.dump(dateTemp, dateSave)

# Print
final = ''
if is_charging:
    final += '⚡️'
elif alertMin or alertMax:
    final += '🔴'
final = '{text}{charge}%'.format(text=final, charge=charge_porcent) 
print(final) 
