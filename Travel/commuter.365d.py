#!/usr/bin/env python
# -*- encoding: utf-8 -*-

import os, sys
import argparse
import subprocess
import simplejson, urllib
import json

fullPathFileName = os.path.realpath(__file__)
commuterdir = "commuter_data"
commuterdir_path = os.path.dirname(os.path.abspath(__file__))+'/'+commuterdir

key_filename = "google-map-api.txt"
key_path = commuterdir_path+'/'+key_filename
origin_filename = "origin.txt"
origin_path = commuterdir_path+'/'+origin_filename
dest_filename = "dest.txt"
dest_path = commuterdir_path+'/'+dest_filename

def run_script(script):
    return (subprocess.Popen([script], stdout=subprocess.PIPE, shell=True).communicate()[0].strip()).replace("'", "’")

# Create plugin data folder if it doesn't exist. 
# This is where input is stored (api key, origin, destination)
commuterdir_exists = os.path.exists(commuterdir_path) 
if not commuterdir_exists:
    os.mkdir( commuterdir_path, 0755 );

debug = False
api_key = ""
orig_coord="Earth"
dest_coord="Mars"
driving_time = ""
driving_dist = ""
origin = ""
destination = ""
commuter_icon = "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"

print "| image={}".format(commuter_icon)
print "---"

# Get inputs from files if they exist
keyfile_exists = os.path.isfile(key_path) 
if keyfile_exists:
    with open(key_path, 'r') as file:
        api_key = file.read()

originfile_exists = os.path.isfile(origin_path) 
if originfile_exists:
    with open(origin_path, 'r') as file:
        orig_coord = file.read()   
        origin = orig_coord  

destfile_exists = os.path.isfile(dest_path) 
if destfile_exists:
    with open(dest_path, 'r') as file:
        dest_coord = file.read()  
        destination = dest_coord

###############################################################################################################################################################################################################################################
# Handle inputs
parser = argparse.ArgumentParser()
parser.add_argument('-k', action='store', dest='localkey',help='Create Google Maps API Key')
parser.add_argument('-o', action='store', dest='localorigin',help='Origin Street Address')
parser.add_argument('-d', action='store', dest='localdest',help='Destination Street Address')
results = parser.parse_args()

if('-k' in sys.argv):    
    cmd = "osascript -e \'set theString to text returned of (display dialog \"Please Enter The Google API Key. \n\nIt will be stored in:\n{}".format(key_path) + "  \" with icon note default answer \"\" buttons {\"OK\",\"Cancel\"} default button 1) \'" 
    api_key = run_script(cmd)
    with open(key_path, 'w') as f:
        f.write(api_key)
    sys.exit(1)
    
if('-o' in sys.argv):    
    cmd = "osascript -e \'set theString to text returned of (display dialog \"Please Enter Origin Street Address. \n\nIt will be stored in:\n{}".format(origin_path) + "  \" with icon note default answer \"\" buttons {\"OK\",\"Cancel\"} default button 1) \'" 
    orig_coord = run_script(cmd)
    with open(origin_path, 'w') as f:
        f.write(orig_coord)
    sys.exit(1)    

if('-d' in sys.argv):    
    cmd = "osascript -e \'set theString to text returned of (display dialog \"Please Enter Destination Street Address. \n\nIt will be stored in:\n{}".format(dest_path) + "  \" with icon note default answer \"\" buttons {\"OK\",\"Cancel\"} default button 1) \'" 
    dest_coord = run_script(cmd)
    with open(dest_path, 'w') as f:
        f.write(dest_coord)
    sys.exit(1)     

###############################################################################################################################################################################################################################################
# Check API Key
if api_key == "" or api_key == "invalid":
    print("🔑 Add API KEY | trim=false, color=red font='Lucida Grande Bold' bash=" + fullPathFileName +  " param1=-k param2=null terminal=false refresh=true")
else:
    #print("🔑 API Key: ✅ " )
    key_tooltip = str(key_path)
    print "🔑 API Key | checked=true color=#179C52 font='Lucida Grande Bold' bash=" + fullPathFileName +  " param1=-k param2=null terminal=false refresh=true tooltip=\"%s\"" % (key_tooltip) 
print "--" + api_key
    
###############################################################################################################################################################################################################################################
# Make API Call to Google
url = "https://maps.googleapis.com/maps/api/distancematrix/json?key={0}&units=imperial&origins={1}&destinations={2}&mode=driving&language=en-EN&departure_time=now&traffic_model=best_guess".format(api_key,str(orig_coord),str(dest_coord))
result= simplejson.load(urllib.urlopen(url))
print "---"
if result['status'] != 'OK':
    #print(result)
    debug = True
    if 'status' in result.keys():
        print("🚫 " + str(result['status']) + ' 🚫| color=red')
    if 'error_message' in result.keys():
        print("" + str(result['error_message']) + '| color=red')
        if str(result['error_message']) == "The provided API key is invalid.":
            api_key = "invalid"
            with open(key_path, 'w') as f:
                f.write(api_key)
            
        
    print("🚀 From: " + origin    + "| trim=false, bash=" + fullPathFileName +  " param1=-o param2=null terminal=false refresh=true")
    print("🏁 To: " + destination + "| trim=false, bash=" + fullPathFileName +  " param1=-d param2=null terminal=false refresh=true")   
         
else:
###############################################################################################################################################################################################################################################
    print "---"
    try:
        driving_time = result['rows'][0]['elements'][0]['duration_in_traffic']['text']
        driving_dist = result['rows'][0]['elements'][0]['distance']['text']
        origin = str(result['origin_addresses'][0])
        with open(origin_path, 'w') as f:
            f.write(origin)
        destination = str(result['destination_addresses'][0])
        with open(dest_path, 'w') as f:
            f.write(destination)
    
    except Exception, e:
        print '❗ Error parsing results- reason "%s" | color=red' % str(e)
        debug = True
        print 'Result:'
        print result
        #sys.exit(1)

    #Print results
    print("🚗 Commute | color=red font='Lucida Grande Bold'")
    print("🚀 From: " + origin + "| trim=false, bash=" + fullPathFileName +  " param1=-o param2=null terminal=false refresh=true")
    print("🏁 To: " + destination+ "| trim=false, bash=" + fullPathFileName +  " param1=-d param2=null terminal=false refresh=true")
    print("⏱️ Driving Time: "+ driving_time)
    print("📏 Driving Dist: "+ driving_dist)
    directions = "https://www.google.com/maps?saddr={}&daddr={}&t=m&z=7&layer=t".format(origin.replace(" ","+").replace(",",""),destination.replace(" ","+").replace(",",""))
    print "🗺️ View in Google Maps" + "| color =#4285F4 font='Lucida Grande Bold' href=%s " % (directions)

###############################################################################################################################################################################################################################################
# When things go wrong...
#debug = True
if debug:
    print "---"
    print "🐞 debug | color=#DB4437"
    json_formatted_str = json.dumps(result, indent=2)
    print('-- Result| color=white')
    for line in json_formatted_str.split('\n'):
        print('----' + line)
    if 'directions' in vars():    
        print('-- Directions URL | color=white')
        print('---- ' + directions + '|  color=#4285F4 href=%s ' % (directions))
###############################################################################################################################################################################################################################################
print "---"
print "🔄 Refresh | refresh=true"


