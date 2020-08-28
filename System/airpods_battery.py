#!/usr/bin/env python3

# <bitbar.title>AirPods Battery</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Mateo Lanz and Eugene Iserovich</bitbar.author>
# <bitbar.author.github>matlanz</bitbar.author.github>
# <bitbar.desc>Displays battery for connected AirPods</bitbar.desc>
# <bitbar.dependencies>python3</bitbar.dependencies>

import os
import sys
import subprocess
import plistlib
import json

# Options for display are Smart, Both, Each, High, Low
#   smart - switches between below option depending on whats connected and battery levels
#   both - displays the battery for both AirPods combined, as long as they're close, else shows Each
#   each - displays the battery for each AirPod, individually
#   high - displays the battery for the pod with the highest percentage
#   low  - displays the battery for the pod with the lowest percentage
all_modes = ['smart', 'both','each','high','low']
selected_mode = 'smart'

if len(sys.argv) > 1:
  mode = sys.argv[1]
  if mode not in all_modes:
    print('Error')
    print('---')
    print('Something went wrong...')
    print('Display Mode is not valid.')
    exit(1)
  
  script_lines = []
  with open(__file__,'r') as f:
    for line in f:
      script_lines.append(line)
  
  if 'selected_mode =' not in script_lines[24]:
    print('Error')
    print('---')
    print('Something went wrong...')
    print('Check script.')
    exit(1)
    
  script_lines[24] = 'selected_mode = \'' + mode + '\'\n'
  selected_mode = mode
  
  with open(__file__,'w') as f:
    for line in script_lines:
      f.write(line)

# Icons for 'both', 'none', 'left', 'right', 'unkn'
icons = {
  'both': 'iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAACXBIWXMAABYlAAAWJQFJUiTwAAAB5ElEQVRYhe1Y0U3DMBA9UCzfX7tBwwR0A8IGGaEjdIQyAekEhA3YAI+QTkDYIP2zlI8gowu4V6e+VAjlI0+yZDn37l7s8+mSm67rYEq4nZSaWZAAg4IUYqoQS4XYKMTOG0YhZjHXzoZsfW5DPtNBoktqPhKtd4nWXWQUIS7xCwF/F+Je66wfmwB/M4J/9lIn156O4h0AjgCwEBz5sbV2yY6qkXLJ7rG11vSLPIdyAPgEgJQIMSwUYu6JyUeISSlW7j/ggtYAULfWuresAo4OAPAEAHvGCc33ZHsI+KkoRs34V117Q45iqMl2FBKu3G2hQlxy5YR7yjHOCc2fLwhZUwx3bG/+A75DzuGK3k6aC/4uGGnuUYwVT40TQa21JQC8CsU4FJQLPd/NCyHXxXilmL8YKGyloIaUFwrj1fygQ3KaJVqbgKM60Xo7xPP4W7LlfOczG+JF+yGW4E1rbaUQN3R1Q6XBcZz92h0HzfviWflHHAK/ZWdwDqj6Fl6w7wp7geYEvCjEwkvabUwMSAR5AR6Etj4WHm8pIcwdYwyzoBhmQTHMgmKYnCBppa6pHeVrf2X/g8ntkOjvB31pfrDlk88XZp8FWt271tr4LsX6moGmywjsjaSZG90P/Tfmax/DtAQBwBchj/O0bHss6gAAAABJRU5ErkJggg==',
  'none': 'iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAACXBIWXMAABYlAAAWJQFJUiTwAAABw0lEQVRYhe1YLY/DMAz1nRrJYGBgYKBg4H7A4P1/dPDgwMBAQcGBggJLrbSTpdcpy5yPDm1SHsoSf7zYTurs43q90ivh86XYVEIFaGIijpmJ6EBEu0BuIKLLJDKkzDvmLfS33vRMRH/QF0vPLGrHfICxFLpJ5Bwh80VEbUZfSV3CyYeUwViOjKJ1zHtDf19ARnGArzghhLlFaEvwYDAyZ2HGpvyUPkRI60Vz+1NIqnHMu+UHxtG69DDDh8BnlNBGhSYRVRgNQzqnee8CHWvcQda0Ax8S6D917AcYykEguwpheHU3O8fchMy9CBwNHWucqqUNfDCugRvCCI0Q+l5RC34UhtLagw8OU3pHaBLpiagvJEO4i24EMO7SKnekevi0CcHoCaRy6K2LDXOl+qdwMtp+RK5+QrFqZJKRcMwt7jQOlpKfnmw/FBT4PImMuI316FpHWnVUfqPpwHgpgdFP8VOEPAfLqVkc/MZ2iegeUeAL6XNsAz5Ki7cxUrdWr8hX7RhzqIRyqIRyqIRyeJ93WQBBO+oj1TWulb/hbSNExlst1VuzIV/SI5VFCM9e3+CQekpjzV/vY0/nEPX/oRwqoSSI6B9p3MAHBbp00wAAAABJRU5ErkJggg==',
  'left': 'iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAACXBIWXMAABYlAAAWJQFJUiTwAAABb0lEQVRYhe2Y0W3CMBCGf6pYvjfYoNmg7QTNCGzQjsAGZYOWCaAbMAIjpBukEzS8neSHVEaOZEUo9pkg5cGfZAnC+fJxXByHRdd1mBMPs7LJQhEkCymitSKqFVHnjUYRfSmiMjVvUlMrogOAt5GQM4CNYT5Ic4srpIi2ARnLEsBeEb1L84sqpIhWAP4iw89O7MUw17HnkFZoHRn3C6B0UrFzLhRCIb9ZdwBad8KnQVxjmFvb9ML8N132DYDTDfOvIq1Q673+HIkrXb89S6WlFTpGxj26Ci7vKmSYG9c7MViZb8MsEhIvjO6nOF1p5CE7w7wRJbdYoZRRaL0ttG4LrbvBOBZaV8l5xd/A3ccA1IZ5pYgq7yN7rB2ZGiRJyF09H4rILnyXtcYwV+Fp9xPqsY37OoVIT94xhshCIbJQiCwUYnZCqSv15DvFntlVKPVB0T5v7f1jhnkxhVBqhexW9sd7H7uLDJL/HwqRhUYB8A/VQrYaHHvClwAAAABJRU5ErkJggg==',
  'right': 'iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAACXBIWXMAABYlAAAWJQFJUiTwAAABX0lEQVRYhe2Y0U3DMBCGf1As3xswAdmAbEBH6AZ0A0ag2aCdAHWDbkA2oN2g3aC8nZSHICNHiiIS+45UipB/yVKc2JfP57N9yU3TNJiTbmdFk4AipAYyRLkh2hiikyFqOuVgiJZau6qgNkQrABsAdyPNdjXzSmpb7CEP8x6AcXoxRGupfZGHDFEB4BPAVwRQq4ea+RL7DqmHlh4mB3AW9ImWJqgPfsSnX54dAZQAtp17+bWBQqoGYKOUKfoUhuh+YORPAD5696LjBwoPVT6YnQceI/vsrwZUMzugnWCFbWtm0fRpN0a3Kb4GmrkAX0iW/I8ckKZk1i4ya/eZtU2vXDJr11q7f86HfIAXbd1Na3uW1cyi+IF2ygbAKn9Z+Bgra2bx0aFZ9kN6nsJIyhhDSkAhJaCQElBIswOacqcue/VqoN2o/rWH3np1VV49pYe6XxpHaeraKv0fCikBjQrAN4FDtHABxJI/AAAAAElFTkSuQmCC',
  'unkn': 'iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAACXBIWXMAABYlAAAWJQFJUiTwAAACnklEQVRYhe1YsY7TQBAdG1uzCT5hpBQpUqS4gjLlgYSUmgqq+wxaPoGPoKC6ggqdKOAPKE9UFBQnlMIFih3phNaKL0aDJtF6vOt1gOKQ/KRV7PWbmbez4/Vugrqu4S4hvFNqBkE9ELkosVIKAOYAMBG8HwCw2mpddLmPlUrZPjW6K7a/3mqtbXbWoo6VmrOzLpCobw4xpwAw89iTqGvZ2ZoyduYTQ5jFSk0t9tMeYghzjuUWxGmecWr7oOXQ0WdDxYMyp7SVIaoXmtvPPUVFsVKT/Q1fO+vSQMUxNMd0CkqItNWaDG4sjqiP5n0lbGzXK+Za/XAMLez/6LUv2JEPmrlHQaaXRjOJlYqkciMDC4uN7bqrlhKOoXgZOEBm6IZJZ0fUgpmFom/tcQwlp7QhaKt1BgCZFHMyvt96vRkrroW9fXUyHvfQcxCVcUy7IHb6lUX9RjIan+b5+s09gFeCmsmFLQiCRZ7nH+B299KnJkFMq7J81HpAK7WtRYjpwzQ955TScl4j4scI8SxCnEkbrq1izw3D8B1zl6ItRqPRU4P7vOHHJUgGMNrbCHEaISY9uQkNjlskuPS78AqSRqjUE0S8FJlKbVy6D8PwwuBeUlZImE+MVZDNiEe3JCHGlFy4AhDf5NK1mCarmJagrgD7OjADGa0RwDYAF1c2+ZbRV/4BAGwAYFnX9ZV8CW4BXiPiJ6Prn3D3aKw3dV2/D4LgBX2DuowoUBiG691u98wXgEVBWZaPfVxwbdAkeIsgPxlXVVlSYbc2WTb+GNV6sym++GL91Z7aJsaFn6X+3oc3HIN8GAT50GfPA7z7kwXctWs8ln/Af5shsJzVuvbWysLPHNwGemWIj72mw6LrKM3PzOeZ6+gsMfw/5MMgqBMA8Asz8s1ebpkV1wAAAABJRU5ErkJggg=='
}

cmd = 'system_profiler -json SPBluetoothDataType 2>/dev/null'
result = subprocess.run(cmd, stdout=subprocess.PIPE, shell=True, check=True).stdout.decode('utf-8')

paired_devices = json.loads(result)['SPBluetoothDataType'][0]['device_title']

airpods_sets = {}

for device in paired_devices:
  name = next(iter(device))
  info = device[name]
  
  minor_type = info.get('device_minorClassOfDevice_string') 
  if minor_type != 'Headphones':
    continue
  
  manufacturer = info.get('device_manufacturer')
  if 'Apple' not in manufacturer:
    continue
  
  paired = info.get('device_ispaired')
  if 'Yes' not in paired:
    continue
  
  mac_address = info.get('device_addr')
  connected = info.get('device_isconnected')
  connection_mode = info.get('device_ConnectionMode')
  
  airpods_sets[name] = {'name': name, 'mac_address': mac_address, 'connected': connected, 'connection_mode': connection_mode}

if len(airpods_sets) == 0:
  print('| templateImage=' + icons['unkn']) # need icone for none / broken 
  print('---')
  print('No paired AirPods found.')
  exit()

airpods_connected = 0
for airpods in airpods_sets.keys():
  if 'Yes' in airpods_sets[airpods]['connected']:
    airpods_connected += 1

if airpods_connected == 0:
  print('| templateImage=' + icons['none'])
  print('---')
  print('AirPods not connected.')
  exit()

for airpods in airpods_sets.keys():
  if 'active' in airpods_sets[airpods]['connection_mode']:
    active_airpods = airpods_sets[airpods]
    break
    
target = active_airpods['mac_address'].lower()

def get_info(target):
  with open('/Library/Preferences/com.apple.Bluetooth.plist','rb') as f:
    data = plistlib.load(f)

  info = data['DeviceCache'][target]

  right = info['BatteryPercentRight']
  left = info['BatteryPercentLeft']
  case = info['BatteryPercentCase']
  
  return right, left, case

def get_high(right, left):

  if right > left:
    high = right
    side = 'right'
  elif left > right:
    high = left
    side = 'left'
  else:
    high = right
    side = 'both'
  batt = f'{high}%'
  return side, batt

def get_low(right, left):

  if right == 0:
    low = left
    side = 'left'
  elif left == 0:
    low = right
    side = 'right'
  elif right < left:
    low = right
    side = 'right'
  elif left < right:
    low = left
    side = 'left'
  else:
    low = right
    side = 'both'

  batt = f'{low}%'  
  return side, batt

def get_each(right, left):

  if right == 0:
    batt_r = ' …'
  else:
    batt_r = f'{right}%'

  if left == 0:
    batt_l = ' …'
  else:
    batt_l = f'{left}%'
    
  batt = f'L:{batt_l} R:{batt_r}'
  side = 'both'
  return side, batt

def get_both(right, left):

  difference_threshold = 5
  difference = abs(right - left)

  if difference > difference_threshold:
    return get_each(right, left)
  else:
    side, both = get_low(right, left)
    side = 'both'
    batt = both
    return side, batt

def get_smart(right, left):
  if right != 0 and left !=0:
    return get_both(right, left)
  else:
    return get_high(right, left)

right, left, case = get_info(target)
  
dispatcher = {'smart': get_smart, 'both': get_both, 'each': get_each, 'high': get_high, 'low': get_low}

side, batt = dispatcher[selected_mode](right, left)
print(f'{batt} | templateImage=' + icons[side])

print('---')

if right != 0:
  print(f'Right: {right}%')
else:
  print('Right not connected.')
  
if left != 0:
  print(f'Left: {left}%')
else:
  print('Left not connected.')
  
if case != 0:
  print(f'Case: {case}%')
  
print('Mode: ' + selected_mode.capitalize())
for mode in all_modes:
  print('--' + mode.capitalize() + ' | bash="' + sys.argv[0] + '" param1="' + mode + '" terminal=false refresh=true')

exit ()