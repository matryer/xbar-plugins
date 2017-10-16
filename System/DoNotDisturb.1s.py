#!/usr/bin/env LC_ALL=en_US.UTF-8 /usr/local/bin/python3
# <bitbar.title>Do Not Disturb</bitbar.title>
# <bitbar.author>Weibing Chen</bitbar.author>
# <bitbar.author.github>weibingchen17</bitbar.author.github>
# <bitbar.desc>Turn on "Do Not Distrub" for certain period. Only for Sierra and High Sierra.</bitbar.desc>
# <bitbar.dependencies>python3</bitbar.dependencies>
# <bitbar.version>v1.0</bitbar.version>
# 
import os,sys
import datetime

def idle():
    print(':bell:')
    print("---")
    print(" 1 min | color=blue bash=" + fullPathFileName +  " param1=5 terminal=false refresh=true")
    print(" 5 min | color=blue bash=" + fullPathFileName +  " param1=5 terminal=false refresh=true")
    print("10 min | color=blue bash=" + fullPathFileName +  " param1=10 terminal=false refresh=true")
    print("30 min | color=blue bash=" + fullPathFileName +  " param1=30 terminal=false refresh=true")
    print("60 min | color=blue bash=" + fullPathFileName +  " param1=60 terminal=false refresh=true")
    print("Custom | color=blue bash=" + fullPathFileName +  " param1=set terminal=false refresh=true")

def touch(a_file):
    with open(a_file, 'a'):
        os.utime(a_file, None)

def setATime(a_time):
    touch(lockFile)
    with open(setFile, 'w') as f:
        f.write(a_time)
    triggerDND()

def cancel():
    triggerDND()
    if os.path.isfile(setFile):
        os.remove(setFile)
    idle()

def alert():
    cancel()
    for _ in range(10):
        os.system('afplay /System/Library/Sounds/Tink.aiff')

def triggerDND():
    # Here is why only Sierra and High Sierra is supported: menu bar 1 is used
    TriggerDND = '''osascript -e 'tell application "System Events"
	tell application process "SystemUIServer"
		try
			if exists menu bar item "Notification Center, Do Not Disturb enabled" of menu bar 1 then
				key down option
				click menu bar item "Notification Center, Do Not Disturb enabled" of menu bar 1
				key up option
			else
				key down option
				click menu bar item "Notification Center" of menu bar 1
				key up option
			end if
		on error
			key up option
		end try
	end tell
end tell' 2>/dev/null
'''
    os.popen(TriggerDND)

lockFile = '/tmp/DoNotDisturb.lock'
setFile = '/tmp/DoNotDisturb.set'
fullPathFileName = os.path.realpath(__file__)

if len(sys.argv) == 1:
    # No new "Do Not Disturb" is requested 
    if not os.path.isfile(setFile):
        idle()
    else:
        with open(setFile, 'r') as f:
            setTime = int(f.read())
        timestamp = datetime.datetime.fromtimestamp(os.path.getmtime(lockFile))
        td = setTime - (datetime.datetime.now() - timestamp).total_seconds()
        if td <= 0: 
            alert()
        else:
            print(':no_bell: Do Not Disturb')
            print("---")
            minute, second = divmod(td, 60)
            if minute < 60:
                print(str(int(minute)) + ':' + '{0:02d}'.format(int(second)))
            else:
                hour, minute = divmod(minute, 60)
                print(str(int(hour)) + ':' + '{0:02d}'.format(int(minute)) + ':' + '{0:02d}'.format(int(second)))
            print("Cancel | color=red bash=" + os.path.realpath(__file__) +  " param1=cancel terminal=false refresh=true")
else:
    # A new "Do Not Disturb" is requested
    if sys.argv[1].isdigit():
        # Use preset period
        setATime(str(int(sys.argv[1]) * 60))
    else:
        # Use custom period
        if sys.argv[1] == 'cancel':
            cancel()
        elif sys.argv[1] == 'set':
            line = '''osascript -e 'Tell application "System Events" to display dialog "How many minutes of freeing yourself from the world? or [hh:]mm:ss " default answer ""' -e 'text returned of result' 2>/dev/null '''
            a_time = os.popen(line).read().strip()
            if ':' not in a_time:
                if a_time.isdigit() and int(a_time) > 0:
                    setATime(str(int(a_time) * 60))
            else:
                try: 
                    hms = [int(i) for i in a_time.split(':')]
                    if len(hms) == 2 and 0 <= hms[0] < 60 and 0 <= hms[1] < 60:
                        setATime(str(hms[0] * 60 + hms[1]))
                    if len(hms) == 3 and 0 <= hms[1] < 60 and 0 <= hms[2] < 60:
                        setATime(str(hms[0] * 60 * 60 + hms[1] * 60  + hms[2]))
                except:
                    line = '''osascript -e 'tell application "System Events" to display dialog "Wrong input format" ' 2>/dev/null '''
                    os.popen(line)
