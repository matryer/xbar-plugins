#!/usr/local/bin/python3

# <bitbar.title>put.io transfers</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Ryan Chiechi</bitbar.author>
# <bitbar.author.github>rchiechi</bitbar.author.github>
# <bitbar.desc>Shows put.io transfers and lists files/folders</bitbar.desc>
# <bitbar.image>https://i.imgur.com/L85lfpv.png</bitbar.image>
# <bitbar.dependencies>Python,Requests</bitbar.dependencies>

import requests
import sys,json,base64

OAUTH_TOKEN="<YOUR_TOKEN_HERE>" # https://put.io/v2/docs/gettingstarted.html
BURL="https://api.put.io/v2" # v2 api base url
PUTIO="https://put.io"
#
# Note: there is very little exception handling. If something
#       goes wrong the script will just crash
#

### Functions ###

def strbytes(B, per=''):
    '''
    Return the given bytes as a human friendly KB, MB, GB, or TB string
    modified from: http://stackoverflow.com/a/31631711
    '''
    B = float(B)
    KB = float(1024)
    MB = float(KB ** 2) # 1,048,576
    GB = float(KB ** 3) # 1,073,741,824
    TB = float(KB ** 4) # 1,099,511,627,776

    if B == 0:
       return '0'
    if B < KB:
       return '{0} {1}{2:s}'.format(B,'Byte' if 0 == B > 1 else 'Bytes',per)
    elif KB <= B < MB:
       return '{0:.2f} KB{1:s}'.format(B/KB,per)
    elif MB <= B < GB:
       return '{0:.2f} MB{1:s}'.format(B/MB,per)
    elif GB <= B < TB:
       return '{0:.2f} GB{1:s}'.format(B/GB,per)
    elif TB <= B:
       return '{0:.2f} TB{1:s}'.format(B/TB,per)

def getdir(pid):
    '''
    List the contents of a put.io file id
    '''
    files = []

    # Check that we are listing a folder
    r = requests.get(BURL+'/files/%s?oauth_token=%s' %(pid,OAUTH_TOKEN)) 
    if json.loads(str(r.content,encoding='utf-8'))['file']['file_type'] != 'FOLDER':
        return files
    # If we are then return a list of files
    r = requests.get(BURL+'/files/list?parent_id=%s&oauth_token=%s' %(pid,OAUTH_TOKEN))
    for f in json.loads(str(r.content,encoding='utf-8'))['files']:
        # Fetch thumbnail icon and convert it to a base64 encoded string
        f['icon'] = str(base64.b64encode(requests.get(f['icon']).content),encoding='utf-8')
        files.append(f)
    return files

###    //     ###

# Print the put.io logo in the menu bar
print('|image='+str(b'iVBORw0KGgoAAAANSUhEUgAAABUAAAAVCAYAAACpF6WWAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAABmJLR0QA/wD/AP+gvaeTAAAAB3RJTUUH4AgFCigacJbY1wAAA6FJREFUOMu9lF1oHFUUgL87s5PNpslmY9BUMNWmaRqSNnW1SNLSmpdKK4it+BBEUPBJEAS1LQVBRKWgBfHFl0LwqVALgg9imiL+QBJsA1WwiTWmbcwvcbPZn+zszu7ce3zY3TEhjfii8zIwZ853v3PuuRf+g0dtFTh8+Ek6Ojvjq6upY8VisUtEokDGcZyJWCw2NHVr6sbIyHf/HvrCiy/1pdKp827OPai1RgQEgcrbsizqIpHRxsbGty5fujj2j9Cenv20tXecSqXT57Tv2wIgsgla/Wbblm5sbDx75/b0R5MTvwQcez30id5Dp1Pp9IdGa0sEnJDimSPNvHLifgaeaqavp4FQyGZ6Lo8xgtbGcvP5ow+0tORv/z41ssn02ZPPH0ylUj9oY2xEiDU4nH+9ld2tlXVFKn8KP00VOfXJDNmcX26HUjoWix0ZHvpqNDB9ZGeb2rat/nPf1zuqZZ577SH2tjmAYLQwt+xRV6uwLdjebLNje5irP6YRBGOM5ft+N8JgJpPGAtjT2RUvlkq9IoKIsK+9lsf3hEGExGqJgbMTnHjjJiffvMnSigcI/fEI7a0O1RzPK/bu3NUeB8rQfD5/XIwEm/BoR22lXGHwy0XuLJRQVoilpOHi138GsXh7CDEGEcGIIee6xwOor3V3dUURob5WAeXExUQJCVqvSGZ0JQaRsOB5hSDP9/3uAKq1jlYDRirGlcQD3dENM9jXEw0WnFlwKeRzGF221VpHAUIARuusshVCGVwtD2DgaBOZnDA+maP/sQaePtQACGuu5ptrScQYvIJLTW0EY0w2gGqtJy3LLpdB1bQ8RrYlvPpcExALRkpE+ODCNOmsD0pRKhUJOTVorSeD8l03d8WI2Wi6zpagHUIqU+L0x79x+epyZcoVSimKXgGvkL8SmE7d+vX6vv3x8VDIOVDtbRVy4Ys5lpNFjDbMLXuM/ZzC9QwoVWEqUArfL43Pzty9vq583+TW1s7UR6PDCmWvh3566Q+yOf/vA7jOrsxUCGjP884YY8yGs59cSdyNRmNWyHH6M2tFHm6x+fZakuGxRAWmKnYVoFIoZYGyKHreewvzs5/d85ZSSlm7dne+E4nUvV1ws5bvl7a0QylEMF6h8P7C/Oy7ImLueUsBklxJfF8TDo+GHGevMfpBpaxNdoLC9/0bbi738tLi/OC6Hd36ki7bWKGm+5r6wuHwMcuyu5SyGoCsETPhed5QMpEYEzE+/9fzF73SG0tTzLiTAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDE2LTA4LTA1VDEwOjQyOjUxKzAyOjAwFs1KTgAAACV0RVh0ZGF0ZTptb2RpZnkAMjAxNi0wOC0wNVQxMDo0MDoyNiswMjowMKwHFVgAAAAASUVORK5CYII=', encoding='utf-8'))

# Everything else goes in menus
print('---')

try:
    # Get transfers and account info as list objects 
    r = requests.get(BURL+'/transfers/list?oauth_token='+OAUTH_TOKEN)
    transfers = json.loads(str(r.content,encoding='utf-8'))['transfers']
    r = requests.get(BURL+'/account/info?oauth_token='+OAUTH_TOKEN)
    info = json.loads(str(r.content,encoding='utf-8'))['info']
except requests.exceptions.ConnectionError as msg:
    print('Error connecting to put.io | color=red')
    sys.exit()
except json.decoder.JSONDecodeError as msg:
    print('JSON Error: see /tmp/putio.log | color=red')
    with open('/tmp/putio.log', 'w') as fh:
        fh.write(str(r.content,encoding='utf-8'))
    sys.exit()

print(':arrows_clockwise: Transfers (up/down) :arrows_clockwise: | color=gray')
for t in transfers:
    # Show a lock for locked torrents
    if t['is_private']:
        print(':lock:',end='')
    else:
        print(':unlock:',end='')
    status = t['status']
    # List seeding torrents in green
    if status == 'SEEDING':
        print('%s | color=green' % t['name'])
    # List downloading torrents in blue
    elif status == 'DOWNLOADING':
        print('%s | color=blue' % t['name'])
    # List everything else in black
    else:
        print('%s | color=black' % t['name'])
    # Print any error messages in red
    if t['error_message']:
        print('%s | color=red' % t['error_message'])
    # Print out the up/down data, peers and speeds
    print('--%s: %s / %s | color=black' % (t['status'].capitalize(),strbytes(t['uploaded']),strbytes(t['downloaded']) ) )
    print('--Peers: %s / %s | color=black' % (t['peers_getting_from_us'],t['peers_sending_to_us'] ) )
    print('--Speed: %s / %s | color=black' % (strbytes(t['up_speed'],'/s') ,strbytes(t['down_speed'],'/s') ))
    # If we are downloading print the ETA and percent complete
    if status == 'DOWNLOADING':
        try:
            print('--ETA: %0.0f min (%s%%) | color=black' % (t['estimated_time']/60,t['percent_done'] ) )
        except TypeError:
            print('--ETA: :x:')
    # Otherwise print the ratio
    else:
        print('--Ratio: %s | color=black' % (t['current_ratio']) )

# Make a divider
print('\n---')

# List the files/folders in the root and recurse two levels deep with submenus
for root in getdir(0):
    print('%s (%s) | color=black image=%s href=%s/files/%s' % (root['name'],strbytes(root['size']),root['icon'],PUTIO,root['id']) )
    for f in getdir(root['id']):
        print('--%s (%s) | color=black image=%s href=%s/files/%s' % (f['name'],strbytes(f['size']),f['icon'],PUTIO,f['id']) )
        for sf in getdir(f['id']):
            print('----%s (%s) | color=black image=%s href=%s/files/%s' % (sf['name'],strbytes(sf['size']),sf['icon'],PUTIO,sf['id']) )

# Make a divider
print('\n---')

# Print disk usage
print('Disk: %s / %s | color=black' % (strbytes(info['disk']['used']),strbytes(info['disk']['size']) ) )

# Print a menu of actions
print('Actions')
print('--Refresh | refresh=true')
print('--Go to put.io | href=%s/transfers' % PUTIO)
# Hit or miss if this works
print('--Clean Transfers | refresh=true terminal=false bash=curl param1="-s" param2="--data oauth_token=%s" param3="--url %s/transfers/clean"' %(OAUTH_TOKEN,BURL))
