#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Transmission Remote
#
# <bitbar.title>Transmission Remote</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Christoph Russ</bitbar.author>
# <bitbar.author.github>christophruss</bitbar.author.github>
# <bitbar.desc>
# Checking transmission status, info and allowing adjustment to alternative speed.
# Requires a single initial setup: `python transmission.4m.py init`
# This plugin displays all not yet completed torrents using "transmission-remote" cli.
# You can pause any torrent in the list by selecting them.
# If all active downloads have completed, but some torrents had been paused, the oldest one will be unpaused automatically.
# </bitbar.desc>
# <bitbar.dependencies>transmission-remote,python,keyring</bitbar.dependencies>
# <bitbar.image>http://i.imgur.com/hUWGq3u.jpg</bitbar.image>
#
# Dependencies:
#   transmission-remote (https://trac.transmissionbt.com/wiki/Building)
#      available via homebrew `brew install transmission`
#   keyring (https://github.com/jaraco/keyring)

import sys
import subprocess
import keyring

def init():
    import getpass
    print "Enter your Transmission Server IP (e.g. 192.168.1.10 or 10.0.1.10): "
    host_ip = raw_input()
    keyring.set_password("transmission-bitbar", "ip", host_ip)
    print "Enter your Transmission port (default: 9091): "
    host_port = raw_input()
    keyring.set_password("transmission-bitbar", "port", host_port)
    print "Enter your Transmission user name: "
    usr = raw_input()
    keyring.set_password("transmission-bitbar", "username", usr)
    print "Enter your Transmission password: "
    pswd = getpass.getpass()
    keyring.set_password("transmission-bitbar", usr, pswd)
    pswd = ''

def check_transmission(argv):
    remote_bin = '/usr/local/bin/transmission-remote' # also make this configurable ?
    host_ip = keyring.get_password("transmission-bitbar", "ip")
    host_port = keyring.get_password("transmission-bitbar", "port")
    host_user = keyring.get_password("transmission-bitbar", "username")
    host_pswd = keyring.get_password("transmission-bitbar", host_user)

    if 'slowdown' in argv:
        subprocess.check_output(remote_bin+' '+host_ip+':'+host_port+' --auth '+host_user+':'+host_pswd+' -as', shell=True)
        exit()
    elif 'speedup' in argv:
        subprocess.check_output(remote_bin+' '+host_ip+':'+host_port+' --auth '+host_user+':'+host_pswd+' -AS', shell=True)
        exit()
    elif 'pause' in argv:
        subprocess.check_output(remote_bin+' '+host_ip+':'+host_port+' --auth '+host_user+':'+host_pswd+' -t ' + argv[-1] + ' -S', shell=True)
    elif 'resume' in argv:
        subprocess.check_output(remote_bin+' '+host_ip+':'+host_port+' --auth '+host_user+':'+host_pswd+' -t ' + argv[-1] + ' -s', shell=True)

    session_info = subprocess.check_output(remote_bin+' '+host_ip+':'+host_port+' --auth '+host_user+':'+host_pswd+' -si -st', shell=True)
    session_info_lines = session_info.split('\n')

    slow_speed = False

    for session_info_line in session_info_lines:
        if 'speed limit' in session_info_line:
            if not 'Unlimited' in session_info_line:
                slow_speed = True

    info = subprocess.check_output(remote_bin+' '+host_ip+':'+host_port+' --auth '+host_user+':'+host_pswd+' -l', shell=True)
    info_lines = info.split('\n')

    host_pswd = ''

    return slow_speed,info_lines

def info_output(slow_speed, info_lines, auto_resume=True):
    unfinished_lines = []
    paused_lines = []

    for info_line in info_lines:
        if not '100%' in info_line and len(info_line)>0:
            if 'Stopped' in info_line:
                paused_lines.append(info_line)
            else:
                unfinished_lines.append(info_line)

    formatting = ''

    if len(unfinished_lines) < 3:
        if len(paused_lines) > 0:
            if not auto_resume:
                # nothing is actively downloading, but there are paused items in your queue
                # red means - warning - you need to check this ...
                formatting = formatting + ' | color=red'
            else:
                # with auto_resume we will automatically resume ONE of the items in the queue
                # it could make sense to resume the smallest download with the simple assumtion that it will download the fastest
                # of course this can and will lead to larger items being stuck in the queue forever
                # it's also more work to actually query all info and parse it to find the smallest item in the paused queue
                # it is a lot easier to just unpause the first (or last) item added to the queue (smallest ID)
                # the items should already be in order, so we just pick the first one !
                t_id = paused_lines[0].strip().split(' ')[0]
                check_transmission([__file__, 'resume', t_id]) # file arg isn't really needed, but it's more consistent this way
                unfinished_lines.insert(1, paused_lines[0]) #this may not be ordered by ID anymore after this step
                paused_lines.pop(0) # or del(0) or [1:]

        else:
            # nothing is actually happening ... all downloads finished
            formatting = formatting + ' | color=green'

    # PRINTING >.<
    if slow_speed:
        print "¶" + formatting # | color=#808080 #grey: 999999 #lighter grey: b3b3b3
    else:
        print "∞" + formatting # | color=white

    print "---"

    if slow_speed:
        print '∞ Unlimited Speed | refresh=true bash=/usr/bin/python param1='+__file__+' param2=speedup terminal=false'
    else:
        print '¶ Limit Speed | refresh=true bash=/usr/bin/python param1='+__file__+' param2=slowdown terminal=false'

    print '---'

    content_formatting = ' | size=12 font=Arial'
    print unfinished_lines[0] + content_formatting

    click_command = ' refresh=true bash=/usr/bin/python param1='+__file__+' param2=pause param3='

    content_formatting = content_formatting + ' color=yellow'
    for info_line in unfinished_lines[1:-1]:
        t_id = info_line.strip().split(' ')[0]
        print info_line + content_formatting + click_command + t_id + ' terminal=false'
    content_formatting = content_formatting[:-13]

    click_command = ' refresh=true bash=/usr/bin/python param1='+__file__+' param2=resume param3='

    if len(paused_lines) > 0:
        content_formatting = content_formatting + ' color=red'
        print '---'
        for paused_line in paused_lines:
            t_id = paused_line.strip().split(' ')[0]
            print paused_line + content_formatting + click_command + t_id + ' terminal=false'
    content_formatting = content_formatting[:-10]

    print '---'
    print unfinished_lines[-1] + content_formatting

def main(argv):
    if 'init' in argv:
        # this is an init call to setup account details, which should only have to be done once
        # simply call:
        # python ../transmission.4m.py init
        # then enter your account details, which will be stored in your keychain !
        # please note for this to work system level access to the created keychain items is REQUIRED
        # technically any other app can thus read out this information from your keychain
        # there is no easy way around this if you do not want to enter your password every time
        init()

    try:
        slow_speed,info_lines = check_transmission(argv)
        info_output(slow_speed,info_lines)
    except Exception as e:
        print "¶ | color=yellow" #error occured
        print "---"
        print "Exception caught. Have you initialized this script?"
        print "python " + __file__ + " init | refresh=true bash=/usr/bin/python param1=" + __file__ + " param2=init terminal=true"
        print "---"
        print e

if __name__ == "__main__":
    main(sys.argv)
