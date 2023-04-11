#!/usr/bin/env python3

# <xbar.title>GoSquared Visitors Online Now Count</xbar.title>
# <xbar.version>v1.1.0</xbar.version>
# <xbar.author>David Barker</xbar.author>
# <xbar.author.github>davidbarker</xbar.author.github>
# <xbar.desc>The total of all the sites you list is displayed. Drop-down menu shows individual visitor count for each site. Clicking on a menu option opens the relevant GoSquared stats page for that site in your browser.</xbar.desc>
# <xbar.image>http://i.imgur.com/04lL6Ox.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://www.gosquared.com/</xbar.abouturl>

# Change these variables (required)
# You can create/view your API key(s) at https://www.gosquared.com/settings/api
api_key = 'demo'
site_keys = {
    'Official Team Fortress Wiki': 'GSN-106863-S',
    'Poolga': 'GSN-181546-E'
}

# Change this variable (optional)
count_suffix = '☻'


# You probably don't need to change anything below this line
import json
import urllib.request, urllib.error, urllib.parse

def get_visitors(site_token):
    try:
        request = urllib.request.Request('https://api.gosquared.com/now/v3/overview?api_key=%s&site_token=%s' % (api_key, site_token))
        response = urllib.request.urlopen(request)
        response = json.load(response)
        return response['visitors']
    except Exception:
        print('Unable to load visitors. Check your site key(s).')

sites = []
for site_name, site_token in site_keys.items():
    visitors = get_visitors(site_token)
    sites.append({'name': site_name, 'token': site_token, 'visitors': visitors})

sites = sorted(sites, key=lambda site: site['name'])
sites = sorted(sites, key=lambda site: site['visitors'], reverse=True)

formatted_sites = []
for site in sites:
    formatted_sites.append(('%s (%s)| href=https://www.gosquared.com/now/%s' % (site['name'], site['visitors'], site['token'])).encode('utf-8'))

print('%s %s' % (sum(site['visitors'] for site in sites), count_suffix))
print('---')
print(b'\n' . join(formatted_sites).decode("utf-8"))
print('---')
