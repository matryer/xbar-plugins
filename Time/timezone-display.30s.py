#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# <xbar.title>Timezone Display</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Ameya Bapat</xbar.author>
# <xbar.author.github>ameyabap</xbar.author.github>
# <xbar.desc>Display current time in multiple timezones with easy timezone selection</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/ameyabap/resources/refs/heads/main/960px-World_Time_Zones_Map.svg.png</xbar.image>
# <xbar.dependencies>python3</xbar.dependencies>
# <xbar.abouturl>https://github.com/ameyabap/resources/blob/main/timezone-xbarapp-plugin.md</xbar.abouturl>

import datetime
import sys
import os

# Timezone configurations with base UTC offsets and DST rules
TIMEZONES = {
    'UTC': {
        'name': 'UTC',
        'offset': 0,
        'dst': False
    },
    'America/New_York': {
        'name': 'New York (EST/EDT)',
        'offset': -5,  # EST
        'dst': True,
        'dst_offset': -4,  # EDT
        'dst_region': 'north_america'
    },
    'America/Chicago': {
        'name': 'Chicago (CST/CDT)',
        'offset': -6,  # CST
        'dst': True,
        'dst_offset': -5,  # CDT
        'dst_region': 'north_america'
    },
    'America/Denver': {
        'name': 'Denver (MST/MDT)',
        'offset': -7,  # MST
        'dst': True,
        'dst_offset': -6,  # MDT
        'dst_region': 'north_america'
    },
    'America/Los_Angeles': {
        'name': 'Los Angeles (PST/PDT)',
        'offset': -8,  # PST
        'dst': True,
        'dst_offset': -7,  # PDT
        'dst_region': 'north_america'
    },
    'Europe/London': {
        'name': 'London (GMT/BST)',
        'offset': 0,  # GMT
        'dst': True,
        'dst_offset': 1,  # BST
        'dst_region': 'europe'
    },
    'Europe/Paris': {
        'name': 'Paris (CET/CEST)',
        'offset': 1,  # CET
        'dst': True,
        'dst_offset': 2,  # CEST
        'dst_region': 'europe'
    },
    'Europe/Berlin': {
        'name': 'Berlin (CET/CEST)',
        'offset': 1,  # CET
        'dst': True,
        'dst_offset': 2,  # CEST
        'dst_region': 'europe'
    },
    'Asia/Tokyo': {
        'name': 'Tokyo (JST)',
        'offset': 9,  # JST
        'dst': False
    },
    'Asia/Shanghai': {
        'name': 'Shanghai (CST)',
        'offset': 8,  # CST
        'dst': False
    },
    'Asia/Kolkata': {
        'name': 'Mumbai/Delhi (IST)',
        'offset': 5.5,  # IST
        'dst': False
    },
    'Asia/Dubai': {
        'name': 'Dubai (GST)',
        'offset': 4,  # GST
        'dst': False
    },
    'Australia/Sydney': {
        'name': 'Sydney (AEST/AEDT)',
        'offset': 10,  # AEST
        'dst': True,
        'dst_offset': 11,  # AEDT
        'dst_region': 'australia'
    },
    'Australia/Melbourne': {
        'name': 'Melbourne (AEST/AEDT)',
        'offset': 10,  # AEST
        'dst': True,
        'dst_offset': 11,  # AEDT
        'dst_region': 'australia'
    },
    'America/Toronto': {
        'name': 'Toronto (EST/EDT)',
        'offset': -5,  # EST
        'dst': True,
        'dst_offset': -4,  # EDT
        'dst_region': 'north_america'
    },
    'America/Vancouver': {
        'name': 'Vancouver (PST/PDT)',
        'offset': -8,  # PST
        'dst': True,
        'dst_offset': -7,  # PDT
        'dst_region': 'north_america'
    },
    'America/Sao_Paulo': {
        'name': 'São Paulo (BRT)',
        'offset': -3,  # BRT
        'dst': False  # Brazil stopped observing DST in 2019
    },
    'Pacific/Auckland': {
        'name': 'Auckland (NZST/NZDT)',
        'offset': 12,  # NZST
        'dst': True,
        'dst_offset': 13,  # NZDT
        'dst_region': 'new_zealand'
    }
}

def get_primary_config_file():
    """Get the path to the primary timezone configuration file"""
    home = os.path.expanduser("~")
    config_dir = os.path.join(home, '.config', 'xbar-timezone')
    os.makedirs(config_dir, exist_ok=True)
    return os.path.join(config_dir, 'primary_timezone.txt')

def load_primary_timezone():
    """Load primary timezone from config file"""
    config_file = get_primary_config_file()
    if os.path.exists(config_file):
        try:
            with open(config_file, 'r') as f:
                primary_tz = f.read().strip()
                return primary_tz if primary_tz in TIMEZONES else 'UTC'
        except:
            pass
    return 'UTC'

def save_primary_timezone(timezone):
    """Save primary timezone to config file"""
    config_file = get_primary_config_file()
    try:
        with open(config_file, 'w') as f:
            f.write(timezone)
    except:
        pass

def get_nth_weekday(year, month, weekday, n):
    """Get the nth occurrence of a weekday in a month"""
    first_day = datetime.date(year, month, 1)
    first_weekday = first_day.weekday()
    
    # Calculate days to the first occurrence of the target weekday
    days_to_weekday = (weekday - first_weekday) % 7
    first_occurrence = first_day + datetime.timedelta(days=days_to_weekday)
    
    # Calculate the nth occurrence
    nth_occurrence = first_occurrence + datetime.timedelta(weeks=n-1)
    
    # Make sure it's still in the same month
    if nth_occurrence.month != month:
        return None
    
    return nth_occurrence

def get_last_weekday(year, month, weekday):
    """Get the last occurrence of a weekday in a month"""
    # Start from the last day of the month and work backwards
    if month == 12:
        last_day = datetime.date(year + 1, 1, 1) - datetime.timedelta(days=1)
    else:
        last_day = datetime.date(year, month + 1, 1) - datetime.timedelta(days=1)
    
    # Find the last occurrence of the weekday
    days_back = (last_day.weekday() - weekday) % 7
    return last_day - datetime.timedelta(days=days_back)

def is_dst_active(utc_datetime, dst_region):
    """Check if DST is active for a given datetime and region"""
    year = utc_datetime.year
    
    if dst_region == 'north_america':
        # North America: Second Sunday in March at 2:00 AM to First Sunday in November at 2:00 AM
        dst_start = get_nth_weekday(year, 3, 6, 2)  # 6 = Sunday
        dst_end = get_nth_weekday(year, 11, 6, 1)
        
        if dst_start and dst_end:
            # Convert to datetime for proper comparison (DST changes at 2 AM local time, which is 7 AM UTC for EST)
            dst_start_dt = datetime.datetime.combine(dst_start, datetime.time(7, 0))  # 2 AM EST = 7 AM UTC
            dst_end_dt = datetime.datetime.combine(dst_end, datetime.time(6, 0))      # 2 AM EST = 6 AM UTC (fall back)
            return dst_start_dt <= utc_datetime < dst_end_dt
    
    elif dst_region == 'europe':
        # Europe: Last Sunday in March at 1:00 AM UTC to Last Sunday in October at 1:00 AM UTC
        dst_start = get_last_weekday(year, 3, 6)  # 6 = Sunday
        dst_end = get_last_weekday(year, 10, 6)
        
        if dst_start and dst_end:
            dst_start_dt = datetime.datetime.combine(dst_start, datetime.time(1, 0))  # 1 AM UTC
            dst_end_dt = datetime.datetime.combine(dst_end, datetime.time(1, 0))      # 1 AM UTC
            return dst_start_dt <= utc_datetime < dst_end_dt
    
    elif dst_region == 'australia':
        # Australia: First Sunday in October to First Sunday in April (next year)
        dst_start = get_nth_weekday(year, 10, 6, 1)
        # For end date, we need to check the next year
        if utc_datetime.month >= 10:
            dst_end = get_nth_weekday(year + 1, 4, 6, 1)
        else:
            dst_end = get_nth_weekday(year, 4, 6, 1)
        
        if dst_start and dst_end:
            # DST changes at 2 AM local time
            if utc_datetime.month >= 10:
                # We're in the DST period that started this year
                dst_start_dt = datetime.datetime.combine(dst_start, datetime.time(16, 0))  # 2 AM AEST = 4 PM UTC (previous day)
                return utc_datetime >= dst_start_dt
            else:
                # We're in the period that might end this year
                dst_end_dt = datetime.datetime.combine(dst_end, datetime.time(16, 0))    # 2 AM AEST = 4 PM UTC (previous day)
                return utc_datetime < dst_end_dt
    
    elif dst_region == 'new_zealand':
        # New Zealand: Last Sunday in September to First Sunday in April (next year)
        dst_start = get_last_weekday(year, 9, 6)
        if utc_datetime.month >= 9:
            dst_end = get_nth_weekday(year + 1, 4, 6, 1)
        else:
            dst_end = get_nth_weekday(year, 4, 6, 1)
        
        if dst_start and dst_end:
            # DST changes at 2 AM local time
            if utc_datetime.month >= 9:
                # We're in the DST period that started this year
                dst_start_dt = datetime.datetime.combine(dst_start, datetime.time(14, 0))  # 2 AM NZST = 2 PM UTC (previous day)
                return utc_datetime >= dst_start_dt
            else:
                # We're in the period that might end this year
                dst_end_dt = datetime.datetime.combine(dst_end, datetime.time(14, 0))    # 2 AM NZST = 2 PM UTC (previous day)
                return utc_datetime < dst_end_dt
    
    return False

def format_time_for_timezone(tz_name):
    """Format current time for a specific timezone"""
    try:
        tz_config = TIMEZONES.get(tz_name)
        if not tz_config:
            return "Error", "Invalid TZ", ""
        
        utc_now = datetime.datetime.utcnow()
        # Determine the correct offset
        if tz_config.get('dst', False):
            dst_region = tz_config.get('dst_region')
            if is_dst_active(utc_now, dst_region):
                offset = tz_config['dst_offset']
                dst_indicator = " (DST)"
            else:
                offset = tz_config['offset']
                dst_indicator = " (STD)"
        else:
            offset = tz_config['offset']
            dst_indicator = ""
        
        # Calculate timezone time
        if isinstance(offset, float):
            # Handle half-hour offsets like India (UTC+5:30)
            hours = int(offset)
            minutes = int((offset - hours) * 60)
            tz_time = utc_now + datetime.timedelta(hours=hours, minutes=minutes)
        else:
            tz_time = utc_now + datetime.timedelta(hours=offset)
        
        # Format: "HH:MM AM/PM"
        time_str = tz_time.strftime("%I:%M %p")
        # Format: "Mon, Jan 30"
        date_str = tz_time.strftime("%a, %b %d")
        
        return time_str, date_str, dst_indicator
    except Exception as e:
        return "Error", f"Invalid TZ", ""

def get_primary_timezone_time():
    """Get time for the primary timezone"""
    primary_tz = load_primary_timezone()
    time_str, date_str, dst_indicator = format_time_for_timezone(primary_tz)
    city_name = TIMEZONES.get(primary_tz, {}).get('name', primary_tz)
    return f" {time_str} {city_name}"

def main():
    # Handle command line arguments for timezone management
    if len(sys.argv) > 1:
        if sys.argv[1] == 'set_primary' and len(sys.argv) > 2:
            # Set primary timezone
            new_primary = sys.argv[2]
            if new_primary in TIMEZONES:
                save_primary_timezone(new_primary)
            return

    # Main display output
    primary_tz = load_primary_timezone()
    
    # Menu bar title - show primary timezone
    print(get_primary_timezone_time())
    print("---")
    
    # Quick timezone switcher for menu bar
    print("🔄 Change Timezone")
    script_path = os.path.abspath(__file__)
    for tz_name in sorted(TIMEZONES.keys()):
        display_name = TIMEZONES[tz_name]['name']
        if tz_name == primary_tz:
            print(f"--✓ {display_name} | color=#007AFF")
        else:
            print(f"--{display_name} | bash='{script_path}' param1=set_primary param2={tz_name} terminal=false refresh=true")
    
    print("---")
    
    # Show all timezones
    print("🌍 World Times")
    for tz_name in sorted(TIMEZONES.keys()):
        time_str, date_str, dst_indicator = format_time_for_timezone(tz_name)
        display_name = TIMEZONES[tz_name]['name']
        
        if tz_name == primary_tz:
            print(f"--✓ {display_name}: {time_str}{dst_indicator} | font=Monaco color=#007AFF")
        else:
            print(f"--{display_name}: {time_str}{dst_indicator} | font=Monaco")
        print(f"----{date_str} | color=#666666 font=Monaco size=11")
    
    print("---")
    print("ℹ️ About")
    print("--Timezone Display Plugin for xbar")
    print("--Updates every 30 seconds")
    print("--✓ Full DST support for major regions")
    print("--Click any timezone above to set as menu bar display")
    print("--Note: DST rules are accurate for 2020+ years")

if __name__ == "__main__":
    main()