#!/usr/bin/env -S PATH="${PATH}:/opt/homebrew/bin:/usr/local/bin" python3

#  <xbar.title>WorldTimeGrid</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Matt Curtis</xbar.author>
#  <xbar.author.github>mrc</xbar.author.github>
#  <xbar.desc>Show UTC time and list of times in different time zones</xbar.desc>
#  <xbar.image>https://github.com/mrc/mrc/blob/74656c8fc28c431eff8cebfe0838b9d7a1f7429f/images/worldtimegrid.png?raw=true</xbar.image>
#  <xbar.dependencies>python3</xbar.dependencies>
#  <--xbar.abouturl></--xbar.abouturl>
#  <xbar.var>number(VAR_HOURS_BACK=12): Number of hours to go back in time list.</xbar.var>
#  <xbar.var>number(VAR_HOURS_FORWARD=12): Number of hours to go back in time list.</xbar.var>
#  <xbar.var>string(VAR_TIME_ZONE_1="UTC"): First time zone in the time list.</xbar.var>
#  <xbar.var>string(VAR_TIME_ZONE_NICK_1=""): Custom nickname for first time zone in the time list.</xbar.var>
#  <xbar.var>string(VAR_TIME_ZONE_2="US/Pacific"): Second time zone in the time list.</xbar.var>
#  <xbar.var>string(VAR_TIME_ZONE_NICK_2="Sunnyvale"): Custom nickname for second time zone in the time list.</xbar.var>
#  <xbar.var>string(VAR_TIME_ZONE_3="Australia/Victoria"): Third time zone in the time list.</xbar.var>
#  <xbar.var>string(VAR_TIME_ZONE_NICK_3="Melbourne"): Custom nickname for third time zone in the time list.</xbar.var>
#  <xbar.var>string(VAR_TIME_ZONE_4="Asia/Kolkata"): Fourth time zone in the time list.</xbar.var>
#  <xbar.var>string(VAR_TIME_ZONE_NICK_4="Kolkata"): Custom nickname for fourth time zone in the time list.</xbar.var>
#  <xbar.var>string(VAR_DEFAULT_ROW_FONT="Menlo"): Row font name. Should be a fixed-width font.</xbar.var>
#  <xbar.var>number(VAR_DEFAULT_ROW_SIZE=10): Row font size.</xbar.var>

import os
from datetime import datetime, timezone, timedelta
from zoneinfo import ZoneInfo

def env_or_default(env_var, typ, default):
    if env_var in os.environ:
        v = os.environ[env_var]
        if type(v) == typ:
            return typ(v)
    return default

var_hours_back = env_or_default('VAR_HOURS_BACK', int, 12)
var_hours_forward = env_or_default('VAR_HOURS_FORWARD', int, 12)

var_time_zone_1 = env_or_default('VAR_TIME_ZONE_1', str, 'UTC')
var_time_zone_2 = env_or_default('VAR_TIME_ZONE_2', str, "US/Pacific")
var_time_zone_3 = env_or_default('VAR_TIME_ZONE_3', str, "Australia/Victoria")
var_time_zone_4 = env_or_default('VAR_TIME_ZONE_4', str, "Asia/Kolkata")
var_time_zone_nick_1 = env_or_default('VAR_TIME_ZONE_NICK_1', str, '')
var_time_zone_nick_2 = env_or_default('VAR_TIME_ZONE_NICK_2', str, 's')
var_time_zone_nick_3 = env_or_default('VAR_TIME_ZONE_NICK_3', str, '')
var_time_zone_nick_4 = env_or_default('VAR_TIME_ZONE_NICK_4', str, '')

var_default_row_font = env_or_default('VAR_DEFAULT_ROW_FONT', str, 'Menlo')
var_default_row_size = env_or_default('VAR_DEFAULT_ROW_SIZE', int, 10)

time_zones = [var_time_zone_1, var_time_zone_2, var_time_zone_3, var_time_zone_4]
time_zone_nicks = [var_time_zone_nick_1, var_time_zone_nick_2, var_time_zone_nick_3, var_time_zone_nick_4]

zones = [ZoneInfo(n) for n in time_zones]

utc_now = datetime.now(timezone.utc)

tz1 = ZoneInfo(var_time_zone_1)
print(f"{utc_now.astimezone(tz1):%H:%M %Z}")
print("---")

default_row_font = var_default_row_font
default_row_size = var_default_row_size
default_row_color = 'gray'
header_font = default_row_font + '-Bold'
now_row_font = default_row_font + '-Bold'
now_row_color = 'blue'
field_sep = ' '

header_time = utc_now.replace(minute=0, second=0, microsecond=0)
alt_header_time = utc_now

width_candidates = time_zone_nicks + ['HH:MM'] + [z.tzname(header_time) for z in zones]
field_width = len(max(width_candidates, key=len)) + 1

nick_header = field_sep.join([f"{n:<{field_width}}" for n in time_zone_nicks])
print(f"{nick_header} | font={header_font} | size={default_row_size} | trim=false")

header = field_sep.join([f"{z.tzname(header_time):<{field_width}}" for z in zones])
print(f"{header} | font={header_font} | size={default_row_size} | trim=false")
print("---")

for hour in range(0 - var_hours_back, 1 + var_hours_forward):
    row_time = header_time + timedelta(hours=hour)
    alt_row_time = alt_header_time + timedelta(hours=hour)
    row_sep = field_sep * (field_width - 4)
    row = row_sep.join([f"{row_time.astimezone(z):%H:%M}" for z in zones])
    alt_row = row_sep.join([f"{alt_row_time.astimezone(z):%H:%M}" for z in zones])
    row_font, row_color = default_row_font, default_row_color
    if hour == 0:
        row_font, row_color = now_row_font, now_row_color
    print(f"{row} | font={row_font} | color={row_color} | size={default_row_size} | trim=false")
    print(f"{alt_row} | font={row_font} | color={row_color} | size={default_row_size} | trim=false | alternate=true")
