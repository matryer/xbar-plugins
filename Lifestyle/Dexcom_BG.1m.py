#!/usr/bin/env -S PATH="${PATH}:/opt/homebrew/bin:/usr/local/bin" python3

# If the script fails to find Python, you may need to change the first
# line to give the explicit path to your Python executable, e.g.
#!/opt/homebrew/bin/python3
# or
#!/Users/username/anaconda3/bin/python

# <xbar.title>Dexcom BG</xbar.title>
# <xbar.version>1.0</xbar.version>
# <xbar.author>Eric Jensen</xbar.author>
# <xbar.author.github>elnjensen</xbar.author.github>
# <xbar.abouturl>https://github.com/elnjensen/dexcom_bg_menubar</xbar.abouturl>
# <xbar.image>https://raw.githubusercontent.com/elnjensen/dexcom_bg_menubar/master/Dexcom_BG_screenshot.png</xbar.image>
# <xbar.dependencies>python, pydexcom</xbar.dependencies>
# <xbar.desc>Displays blood glucose data from Dexcom Share</xbar.desc>
# <xbar.var>string(VAR_USERNAME=""): Your Dexcom username.</xbar.var>
# <xbar.var>string(VAR_PASSWORD=""): Your Dexcom password.</xbar.var>
# <xbar.var>string(VAR_REGION="us"): Set to "jp" if in Japan, or "ous" if you are in a different country outside the United States.</xbar.var>
# <xbar.var>boolean(VAR_MMOL=false): Display blood glucose values in mmol/L instead of mg/dL.</xbar.var>

import json
import os
import tempfile
from datetime import datetime, timezone
# We will do all datetime math in UTC:
UTC = timezone.utc

try:
    from pydexcom import Dexcom, GlucoseReading
    from pydexcom.errors import AccountError, SessionError
except ImportError:
    print("pydexcom not installed|color=red")
    print("---")
    print("Please run 'pip3 install pydexcom'.")
    exit()

# Get the Dexcom Share credentials from environment variables:
username = os.getenv('VAR_USERNAME', '')
password = os.getenv('VAR_PASSWORD', '')
# Get and validate region:
valid_regions = ['us', 'ous', 'jp']
region = os.getenv('VAR_REGION', 'us').lower()
if region not in valid_regions:
    print(f"Invalid region '{region}'|color=red")
    print("---")
    print(f"VAR_REGION must be one of: {', '.join(valid_regions)}")
    exit()

# Check on mmol/L setting:
mmol_string = os.getenv('VAR_MMOL', 'false').lower()
if mmol_string in ['true', '1', 'yes']:
    mmol = True
else:
    mmol = False

# Make sure we really got valid values:
if (username == '') or (password == ''):
    print("Set credentials|color=red")
    print("---")
    print("Open the xbar plugin browser to set your Dexcom Share login credentials.")
    exit()


# File for caching BG values
bg_filename = os.path.join(tempfile.gettempdir(), 'latest_bg_values.json')

def bg_values_to_file(bg_list, filename):
    '''Take a list of GlucoseReading objects, 
    serialize to JSON, and write to provided
    filename.'''

    # Handle the case with a single value for input: 
    if not isinstance(bg_list, list):
        iter_list = [bg_list]
    else:
        iter_list = bg_list
        
    with open(filename, 'w') as f:
        for b in iter_list:
            f.write(json.dumps(b.json)+"\n")

            
def bg_values_from_file(filename):
    '''Given an input filename, read the 
    contents, assumed to be JSON serialized
    BG data from Dexcom Share.  Return a
    list of GlucoseReading objects.
    Assumes that checking for file 
    existence happens elsewhere. '''          
    
    # Reading from file
    b_list = []
    with open(filename, 'r') as f:
        for line in f:
            b_list.append(GlucoseReading(json.loads(line)))

    return b_list

        

now = datetime.now(UTC)
# See if we can get BG values from file:
have_old_bgs = False
if os.path.exists(bg_filename):
    old_bgs = bg_values_from_file(bg_filename)
    have_old_bgs = True
    bg_age = now - old_bgs[0].datetime.astimezone(UTC)
    bg_age_minutes = bg_age.total_seconds()/60
    
if have_old_bgs and (bg_age_minutes < 4):
    # No need to fetch new bgs:
    bgs = old_bgs
    fetch_error = None
else:
    # Fetch new data.
    try:
        dexcom = Dexcom(username=username, password=password, region=region)
        bgs = dexcom.get_glucose_readings(max_count=6)
        if not bgs:
            raise ValueError("No BG data returned from Dexcom")
        fetch_error = None
    except (AccountError, SessionError) as e:
        print("Dexcom auth error|color=red")
        print("---")
        print("Check your username, password, and region setting.")
        print(f"Error: {e}")
        exit()
    except Exception as e:
        fetch_error = f"Fetch failed at {now.astimezone().strftime('%H:%M')}: {e}"
        if have_old_bgs:
            bgs = old_bgs
        else:
            print("No BG data available|color=red")
            print("---")
            print(f"Fetch failed: {e}")
            exit()

# Cycle over list to print output; even for old
# BG data we are updating the age displayed: 
for i in range(len(bgs) - 1):
    bg = bgs[i]
    if mmol:
        bg_value = bg.mmol_l
        bg_diff = f"{bg.mmol_l - bgs[i+1].mmol_l:+0.1f}"
    else:
        bg_value = bg.value
        bg_diff = f"{bg.value - bgs[i+1].value:+0.0f}"
    time_diff = now - bg.datetime.astimezone(UTC)
    time_diff_min = time_diff.total_seconds()/60
    print(f"{bg_value} ({bg_diff}) {bg.trend_arrow} ({time_diff_min:0.0f}m)")
    if i==0:
        # Separator to make earlier values appear in the submenu:
        print("---")
        if fetch_error:
            print(f"{fetch_error}|color=red")

# Save BG values to file if possible. If an
# exception is raised, flag it but exit normally,
# since this caching isn't essential for the script
# to work:
try:
    bg_values_to_file(bgs, bg_filename)
except Exception as e:
    print(f"Failed to write BGs to {bg_filename}:|color=red")
    print(f"{e}.|color=red")
