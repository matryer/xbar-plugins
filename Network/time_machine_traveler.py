#!/opt/homebrew/bin/python3
# -*- coding: utf-8 -*-
# <xbar.title>Time Machine Traveler Helper</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Pavel Zhovner</xbar.author>
# <xbar.author.github>zhovner</xbar.author.github>
# <xbar.image>https://user-images.githubusercontent.com/774290/132701329-36b01255-50f4-4902-8ea3-0088edb38b2b.jpg</xbar.image>
# <xbar.desc>Helps run remote Time Machine. Test network speed to SMB server and start backup only if SMB server is speed enough.</xbar.desc>
# <xbar.dependencies>python3,iperf3,osascript</xbar.dependencies>
#
#<xbar.var>string(SMB_SHARE_ADDRESS=""): Your SMB share address</xbar.var>
#<xbar.var>string(WORKGROUP_NAME=""): Your SMB share address</xbar.var>
#<xbar.var>string(SMB_MOUNT_PATH=""): Your SMB share address</xbar.var>
#<xbar.var>string(SMB_USER=""): Your SMB share address</xbar.var>
#<xbar.var>string(SMB_SHARE_PATH=""): Your SMB share address</xbar.var>
#<xbar.var>string(SPEED_TEST_SERVER=""): Your SMB share address</xbar.var>
#<xbar.var>string(SPEED_TEST_DURATION=""): Your SMB share address</xbar.var>
#<xbar.var>string(SPEED_TEST_TIMEOUT=""): Your SMB share address</xbar.var>
#<xbar.var>string(MIN_SPEED=""): Your SMB share address</xbar.var>
#<xbar.var>string(MAX_LOAD_AVERAGE=""): Your SMB share address</xbar.var>


### Requirements
# brew install python3 iperf3
# pip3 install osascript

### How to use:
# 1. Mount remote SMB share and save credentials in system keychain
# 2. Configure Time Machine to SMB share in System Preferences. Make sure that first backup is created correctly
# 3. Disable automatic backup in Time Machine
# 4. Setup this script 

import subprocess
import json
import glob 
import os
import time
import sys
import datetime

# If you still have this error after install osascript, try to set right python3 shell bang
try:
    import osascript
except ImportError:
    generate_output('''Can't import osascript module
Run: pip3 install osascript''', status='FATAL_ERROR')


def generate_output(message, status):

    if status == 'FATAL_ERROR':
        print("⚠️")
        print("---")
        print("Time Machine Travel Helper | font=LucidaGrande-Bold")
        print("---")
        print("Fatal error:")
        print(message)

    if status == 'IDLE':
        print("🕒")
        print("---")
        print("Time Machine Travel Helper | font=LucidaGrande-Bold")
        # TODO: Show latest backup date
        print("Last Run: " + datetime.datetime.now().strftime("%d %b %Y - %H:%M") + " | size=10")
        print("---")
        print(message)
        print("Skipping backup...")

    if status == 'RUN':
        print("🌀")
        print("---")
        print("Time Machine Travel Helper | font=LucidaGrande-Bold")
        print("---")
        # TODO: Show backup status and progress
        print("Backuping...")

    # Manually run button
    print("---")
    print("Run now | refresh=true")

    # Trying to print some logs if we have some
    try:     
        # Output log in dropdown menu
        print("Logs")
    
        # Current time machine path settings
        print("-- Time Machine Settings:")
        for l in TMUTIL_SETTINGS.splitlines():
            print("-- " + l)

        # SMB status

        # Display upload speed
        if UPLOAD_SPEED:
            print("-- ")
            print("-- Speed Test to: " + SPEED_TEST_SERVER)
            print("-- ====================================================")
            print("-- Upload Speed: " + str(UPLOAD_SPEED) + " Mbits/sec")
        
        # List Time Machine bundles files
        print("-- ")
        print("-- Found " + str(len(TIME_MACHINE_BACKUPS)) + " Time Machine files:")
        print("-- ====================================================")
        for l in TIME_MACHINE_BACKUPS:
            print("-- " + str(l))
    except:
        pass

    # Stop script after print logs
    # we need to always exit with 0 because of xbar
    exit(0)



# Settings VARIABLES
# ---------------------------------------------------------------

SMB_SHARE_ADDRESS = os.environ["SMB_SHARE_ADDRESS"]
WORKGROUP_NAME    = os.environ["WORKGROUP_NAME"]
SMB_MOUNT_PATH    = os.environ["SMB_MOUNT_PATH"]
SMB_USER          = os.environ["SMB_USER"]
SMB_SHARE_PATH    = os.environ["SMB_SHARE_PATH"]

SPEED_TEST_SERVER     = os.environ["SPEED_TEST_SERVER"]
SPEED_TEST_DURATION   = os.environ["SPEED_TEST_DURATION"]
SPEED_TEST_TIMEOUT    = os.environ["SPEED_TEST_TIMEOUT"]

MIN_SPEED        = os.environ["SPEED_TEST_TIMEOUT"]
MAX_LOAD_AVERAGE = int(os.environ["SPEED_TEST_TIMEOUT"])

# ---------------------------------------------------------------


#### Script exit status to draw correct icon
STATUS = ""

#### Print Time Mahcine settings
TMUTIL_SETTINGS_SUB = subprocess.run(["tmutil", "destinationinfo"], capture_output=True)
TMUTIL_SETTINGS = (str(TMUTIL_SETTINGS_SUB.stdout, 'utf-8'))
# Stop if Time Machine not configured
if "No destinations configured" in TMUTIL_SETTINGS:
    generate_output("Time Machine not configured",status='FATAL_ERROR')


#### Check Load Average. Exit if system busy
LA = round(os.getloadavg()[0])
if LA > MAX_LOAD_AVERAGE:
    generate_output("System load is to high.",status='IDLE')


#### Check if Time Machine running
# exit if backup running
#
# TODO: Display running progress
#
# Use python tmutil function if need more info from running time machine process
# https://gist.github.com/andrewbenson/cc5fd79ff6999f0524b8979fe17937a3
#
TMUTIL_PHASE_SUB = subprocess.run(["tmutil", "currentphase"], capture_output=True)
TMUTIL_PHASE = str(TMUTIL_PHASE_SUB.stdout, 'utf-8').strip()
if TMUTIL_PHASE != "BackupNotRunning":
    generate_output("Backup is running...",status='RUN')


#### Check if SMB share availible 
SMBUTIL_SUB = subprocess.run(["smbutil", "status", SMB_SHARE_ADDRESS], capture_output=True)
SMB_CHECK_RESULT = str(SMBUTIL_SUB.stdout, 'utf-8').strip()

# Check if SMB share connect have workgoup name
# Oterwise stop because SMB share not reacheble
if WORKGROUP_NAME not in SMB_CHECK_RESULT:
    generate_output("SMB " + SMB_SHARE_ADDRESS + " not availible.",status='IDLE')


#### Run Network Speed Test
# ---------------------------------------------------------------
try:
    IPERF_SUB = subprocess.run(
        ["/opt/homebrew/bin/iperf3",
        "--connect-timeout", SPEED_TEST_TIMEOUT, # Drop connection in laggy network
        "--time", SPEED_TEST_DURATION, # Run speed test shorter then default 10 seconds
        "--json", # Output in JSON format
        "--client", SPEED_TEST_SERVER], # Connects to Speed Test server in client mode
        capture_output=True)
except:
    generate_output("iperf3 failed to run",status='FATAL_ERROR')

# Convert iperf3 output to valid JSON
IPERF_RESULT = str(IPERF_SUB.stdout, 'utf-8') # Convert bytes to string
IPERF_JSON = json.loads(IPERF_RESULT)
# Exit if error key found 
if "error" in IPERF_JSON:
    generate_output("iperf3: " + IPERF_JSON["error"],status='FATAL_ERROR')

# Else calculate the upload speed and decide if it's enough to start Time Machine
else:
    USPEED_FLOAT = IPERF_JSON["end"]["sum_sent"]["bits_per_second"]
    UPLOAD_SPEED = round(USPEED_FLOAT) // 1000000
    if int(UPLOAD_SPEED) < int(MIN_SPEED):
        generate_output("Internet is too slow: " + str(UPLOAD_SPEED) + " Mbits/sec\n" + "Minimum is: " + str(MIN_SPEED) + " Mbits/sec",status='IDLE')      


#### Mounting SMB share if not mounted. Using osascript (hardest part)
if not os.path.isdir(SMB_MOUNT_PATH):
    #print(SMB_MOUNT_PATH + " not founded. Trying to mount...")
    OSA_ARGS = 'tell application "Finder" to mount volume ' + '"smb://' + SMB_USER + '@' + SMB_SHARE_ADDRESS + '/' + SMB_SHARE_PATH + '"'
    osacode,osaout,osaerr = osascript.run(OSA_ARGS)
    #time.sleep(1)

# Looking for a SMB Path again after mount 
if not os.path.isdir(SMB_MOUNT_PATH):
    generate_output("Mount " + SMB_MOUNT_PATH + " Failed",status='FATAL_ERROR')
    #print("Command used to mount via osascript: " + str(OSA_ARGS))


#### Looking for *.sparsebundle files on SMB share
# exit if not founded
# Create Time Machine backup manually before using this script
TIME_MACHINE_BACKUPS = glob.glob(SMB_MOUNT_PATH + "/*.sparsebundle")
if not TIME_MACHINE_BACKUPS:
    print_error("Time Machine files not found on SMB share.\n Run Time Machine first time manually before using this script")

#### Run Time Machine backup
TIME_MACHINE_SUB = subprocess.run(
     ["tmutil",
     "startbackup"],
     capture_output=True)

if TIME_MACHINE_SUB.returncode != 0:
     #print("ERROR: Time Machine Starting Failed!")
     #osacode,osaout,osaerr = osascript.run('display notification "⚠️ ERROR: Time Machine Starting Failed!" with Title "Time Machine Helper"')
     print_error('Time Machine start failed\n run: "tmutil startbackup" in terminal')

### Check if Time Machine really starts backup
TMUTIL_PHASE_SUB = subprocess.run(["tmutil", "currentphase"], capture_output=True)
TMUTIL_PHASE = str(TMUTIL_PHASE_SUB.stdout, 'utf-8').strip()
if 'BackupNotRunning' in TMUTIL_PHASE:
    generate_output('Time Machine not started\n Run manually: \"tmutil startbackup\" in terminal',status='FATAL_ERROR')

# Generate output
# ---------------------------------------------------------------

generate_output("",status='RUN')