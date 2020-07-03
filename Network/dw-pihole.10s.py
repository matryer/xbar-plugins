#!/usr/bin/env python
# -*- coding: utf-8 -*-
# <bitbar.title>Pi-hole Status</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Albert Steiner</bitbar.author>
# <bitbar.author.github>alst-ovh</bitbar.author.github>
# <bitbar.desc>Show Summary and Manage Pi-hole from Menubar.</bitbar.desc>
# <bitbar.abouturl>https://github.com/alst-ovh/bitbar-dw-pihole/blob/master/dw-pihole.png</bitbar.abouturl>
import json
import os
import ssl
import sys

try:
    _create_unverified_https_context = ssl._create_unverified_context
except AttributeError:
    # Legacy Python that doesn't verify HTTPS certificates by default
    pass
else:
    # Handle target environment that doesn't support HTTPS verification
    ssl._create_default_https_context = _create_unverified_https_context


try:  # Python 3
    from urllib.request import urlopen
except ImportError:  # Python 2
    from urllib2 import urlopen  # noqa


PLUGIN_PATH = os.path.join(os.getcwd(), __file__)

# ---
# Variables
# ---

# URL to the pi-hole admin path without trailing slash
base_url = "http://pi.hole"

# Your Pi-hole API token hash (used for management)
# THIS IS NOT YOUR PIHOLE ADMIN PASSWORD
# You can find this API token on the Admin Page -> Settings -> Api / Web interface -> Show API api_token
api_token = ""

# Menubar icon
icon_enabled = 'iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAActpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyI+CiAgICAgICAgIDx4bXA6Q3JlYXRvclRvb2w+d3d3Lmlua3NjYXBlLm9yZzwveG1wOkNyZWF0b3JUb29sPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KGMtVWAAAA2dJREFUWAnFlluITWEUx4dmGPdcHlA8yClK1MyTS03G0xByV5TEeCKKlAYRkoaUJ0Lz4FbkUngwNC8uDUoI5TrRFHIZzCAzZvj92auWbZ8znLPPmVW/+da3vvWtteY7a3975+W1Lz3bd8mdxwlS/YDPcArKoEOliewqyHOReaKjqqoNFWOFNWDvkNNakqQgFdYMpZBM1HtFUJDMIR27gt0FO5nw+I61QaHA3Zjvhq8g/+MQqxQTzYKHC9J8u8tWiH4VvN8ltx6buohIbaFElvSey7IuwmeBW49VXUG0qKJkGwz6eevBCtVYDVmVWUT/AD6pdPXMypD9BvP+kHUZSIZKuA/fIVycLtEq6AU5FxV3EHxRi3NehUvYGX0M+N56xVyF5lTUwFvgPfjTMf0m9u6QE1GjXgFLrvEyqLEbnP08ehfIquhW1r1jxXxD13VgkkB5CbZ+Bl2nmRXRyejJsmSv0SdEZJrjfOR7EmIvSv3g3/yfmI+FKJmJUYWcDkbp+q7Kh1ikE1H0grST8eND7AdgFSwD9VEjnIO54H2PMddTmbFUEMECP0CfDuWgYsweHieytjdifSu2jEQ/Swv4hK3Mj4DeXypM945f1ymp3z6CPl1GwnOQj+4rnVxa0pVdT0CB3sAG0Ju7BmR7Ckqs/poNq6EEJCpKPvamnxfMZXsLA+C/ZS07FEAooMk4FLNvMqMbh6E3g3yGBHb1of+J9wT2v4ZUTbbUeT9zuj7WTBKmuFGnUhDMdSdJVNy1X9rvPzOc/oeaqiDvOMpNRjj9kdNNHW4KY6HTeztd/RUpqQqqcjumOl0Xn0SfHmrusOinMRkfKP0YJ5mRcb/T/1nVf1cHOm49HWrqzcFctn2gd9VOeAxnQTd3X9DJyecFrIHrwVw2+ab9jiths05CgTyNzPWJsStkV+Fq2KFwCFrB72throciI9nIbh9U+jYohXBC86tnbT2UQx2YvQI9Y9EjexQsqEZdmLq1lXgK9IA+MB/0M3lf0w9jV6xYRJdkDVhwff9IXw5hWYjB/GzUXsWIVXQKvigl2xGRIdxX2qO9WRG9JqrB/vMm9CKXqRj9i1u/gK49WZV8ousU2kCF6bt6WoB9Y2utEuSbM5lMpttgp2XjLWxl6VaRaddrv5640UEBdxhrQcWlJT8BdaxGflEnmuwAAAAASUVORK5CYII=='  # noqa
icon_disabled = 'iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAE92lUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPD94cGFja2V0IGJlZ2luPSLvu78iIGlkPSJXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQiPz4KPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNS41LjAiPgogPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iCiAgICB4bWxuczp4bXA9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8iCiAgICB4bWxuczpleGlmPSJodHRwOi8vbnMuYWRvYmUuY29tL2V4aWYvMS4wLyIKICAgIHhtbG5zOnBob3Rvc2hvcD0iaHR0cDovL25zLmFkb2JlLmNvbS9waG90b3Nob3AvMS4wLyIKICAgIHhtbG5zOnhtcE1NPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvbW0vIgogICAgeG1sbnM6c3RFdnQ9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZUV2ZW50IyIKICAgdGlmZjpJbWFnZUxlbmd0aD0iMzYiCiAgIHRpZmY6SW1hZ2VXaWR0aD0iMzYiCiAgIHRpZmY6T3JpZW50YXRpb249IjEiCiAgIHRpZmY6UmVzb2x1dGlvblVuaXQ9IjIiCiAgIHRpZmY6WFJlc29sdXRpb249IjE0NC4wIgogICB0aWZmOllSZXNvbHV0aW9uPSIxNDQuMCIKICAgeG1wOkNyZWF0b3JUb29sPSJ3d3cuaW5rc2NhcGUub3JnIgogICB4bXA6TW9kaWZ5RGF0ZT0iMjAyMC0wNy0wMlQxODozODoyNiswMjowMCIKICAgeG1wOk1ldGFkYXRhRGF0ZT0iMjAyMC0wNy0wMlQxODozODoyNiswMjowMCIKICAgZXhpZjpQaXhlbFhEaW1lbnNp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'  # noqa

# Url to check the service status
url_status = "%s/api.php?status&auth=%s" % (base_url, api_token)

# Url to get the summary
url_summary = "%s/api.php?summary" % base_url

# Urls to enable/disable service
url_enable = "%s/api.php?enable&auth=%s" % (base_url, api_token)
url_disable = "%s/api.php?disable&auth=%s" % (base_url, api_token)
url_disable_10s = "%s/api.php?disable=10&auth=%s" % (base_url, api_token)
url_disable_30s = "%s/api.php?disable=30&auth=%s" % (base_url, api_token)
url_disable_5m = "%s/api.php?disable=300&auth=%s" % (base_url, api_token)
url_disable_1h = "%s/api.php?disable=3600&auth=%s" % (base_url, api_token)

# Url to get versions
url_versions = "%s/api.php?versions" % base_url

# Text Colors
c_txt = '#000000'
c_ok = '#008000'
c_warn = '#FF0000'

# Online
online = False

# ---
# Helper methods
# ---


def do_url_silent(url):
    try:
        urlopen(url)
    except Exception as e:
        print('Script error:')
        print(e)


def is_online():
    try:
        r = urlopen(url_summary).getcode()
    except:
        r = 0
    return r == 200


def convert_to_native(data):
    return json.loads(data)


def do_request(url, method='GET'):
    response = urlopen(url)
    return convert_to_native(response.read())


def get_summary():
    response = do_request(url_summary)
    return response


def get_status():
    response = do_request(url_status)
    return response['status']


def get_versions():
    response = do_request(url_versions)
    return response


def separator():
    print('---')


# Layout Offline
def bitbar_offline():
    icon_type = 'disabled'
    print("Offline| templateImage= %s" % globals()['icon_%s' % icon_type])
    separator()
    print('Status: %s | color=%s' % ('Offline', c_warn))


# Layout Online
def bitbar_online():
    summary = get_summary()
    versions = get_versions()
    status = get_status()
    enabled = status == 'enabled'
    # Menubar Icon
    if enabled:
        icon_type = 'enabled'
    else:
        icon_type = 'disabled'
    print("%s / %s (%s %%) | templateImage= %s" % (summary['dns_queries_today'], summary['ads_blocked_today'], summary['ads_percentage_today'], globals()['icon_%s' % icon_type]))
    separator()

    if enabled:
        print('Status: %s | color=%s' % ('Enabled', c_ok))
    else:
        print('Status: %s | color=%s' % ('Disabled', c_warn))
    if not versions['core_update']:
        print("Pi-hole Version: %s | color=%s" % (versions['core_current'], c_txt))
    else:
        print("Pi-hole Version: %s Update available %s| color=%s" % (versions['core_current'], versions['core_latest'], c_warn))
    if not versions['web_update']:
        print("Web Interface Version: %s | color=%s" % (versions['web_current'], c_txt))
    else:
        print("Web Interface Version: %s Update available %s| color=%s" % (versions['web_current'], versions['web_latest'], c_warn))
    if not versions['FTL_update']:
        print("FTL Version: %s | color=%s" % (versions['FTL_current'], c_txt))
    else:
        print("FTL Version: %s Update available %s| color=%s" % (versions['FTL_current'], versions['FTL_latest'], c_warn))
    separator()
    print("Blocked: %s (%s%%) | color=%s" % (summary['ads_blocked_today'], summary['ads_percentage_today'], c_txt))
    print("Queries: %s | color=%s" % (summary['dns_queries_today'], c_txt))
    print("Queries Cached: %s | color=%s" % (summary['queries_cached'], c_txt))
    print("Queries Forwarded: %s | color=%s" % (summary['queries_forwarded'], c_txt))
    print("Unique Domains: %s | color=%s" % (summary['unique_domains'], c_txt))
    print("Blocklist: %s | color=%s" % (summary['domains_being_blocked'], c_txt))
    separator()

    if enabled:
        print('Disable Pi-hole')
        print('--10 Seconds | terminal=false refresh=true bash=%s param1=url-silent param2="%s"' % (__file__, url_disable_10s))
        print('--30 Seconds | terminal=false refresh=true bash=%s param1=url-silent param2="%s"' % (__file__, url_disable_30s))
        print('--5 Minutes | terminal=false refresh=true bash=%s param1=url-silent param2="%s"' % (__file__, url_disable_5m))
        print('--1 Hour | terminal=false refresh=true bash=%s param1=url-silent param2="%s"' % (__file__, url_disable_1h))
        print('--Permanently | terminal=false refresh=true bash=%s param1=url-silent param2="%s"' % (__file__, url_disable))
    else:
        print('Enable Pi-hole | terminal=false refresh=true bash=%s param1=url-silent param2="%s"' % (__file__, url_enable))

    print("Admin Console | href=%s" % base_url)
    separator()


# Bitbar Layout Switch
def bitbar():
    if is_online():
        bitbar_online()
    else:
        bitbar_offline()


# Switch if called with Params
param = ''
if len(sys.argv) > 2:
    param = sys.argv[1]
    url_cmd = sys.argv[2]

# Call Url
if param == 'url-silent':
    do_url_silent(url_cmd)
else:
    # Execution
    try:
        bitbar()
    except Exception as e:
        print('Script error:')
        print(e)
        separator()
