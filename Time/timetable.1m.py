#!/usr/bin/python3

# xbar meta
# <xbar.title>Timetable</xbar.title>
# <xbar.version>v2.0</xbar.version>
# <xbar.author>Cabin Zhu</xbar.author>
# <xbar.author.github>cabinz</xbar.author.github>
# <xbar.desc>Display the current event from a given daily timetable. To make it work, make sure to configure the path to your CSV timetable file. More about the user manual, see project page https://github.com/cabinz/xbar-timetable.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/cabinz/xbar-timetable/main/docs/timetable.png</xbar.image>
# <xbar.abouturl>https://github.com/cabinz/xbar-timetable</xbar.abouturl>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.var>string(VAR_TIMETABLE_FILE="/path/to/your/timetable.csv"): An absolute path to a CSV file of the timetable to be displayed.</xbar.var>
# <xbar.var>boolean(VAR_DISPLAY_EMOJI_WHEN_GIVEN=true): Display in the menu bar the emoji instaad when given.</xbar.var>
# <xbar.var>boolean(VAR_DISPLAY_TIME_LEFT=true): Display in the menu bar the remaining time of the current event. Otherwise it will be in the pop-up menu.</xbar.var>
# <xbar.var>string(VAR_FONT='Monaco'): Display font in the pop-up menu.</xbar.var>
# <xbar.var>number(VAR_FONT_SIZE=15): Display font size in the pop-up menu.</xbar.var>

import datetime
import csv
import os
import sys
from typing import List


# If you are on SwiftBar or other platforms w/o user-configurable variables,
# manually change the assignment as a string of absolute path to your timetable CSV file.
CSV_TIMETAB = os.environ.get("VAR_TIMETABLE_FILE")
DISP_EMJ = os.environ.get("VAR_DISPLAY_EMOJI_WHEN_GIVEN", 'true') == 'true'
DISP_TIME_LEFT = os.environ.get("VAR_DISPLAY_TIME_LEFT", 'true') == 'true'
FONT = os.environ.get("VAR_FONT", 'Monaco')
FONT_SIZ = int(os.environ.get("VAR_FONT_SIZE", '15'))

RGB_LIGHT_GREY = "#848481" 
RGB_ORANGE = "#F2980B"


def to_m_time(time: str):
    """Convert HH:MM time to minutes since 00:00."""
    hour, minute = map(int, time.split(':'))
    return hour * 60 + minute


def to_timestamp(m_time: int):
    assert 0 <= m_time <= to_m_time("24:00") 
    hr, minute = m_time // 60, m_time % 60
    return f"{hr:02d}:{minute:02d}"


def exit_with_error(err_msg: str):
    print('⚠️Error')
    print('---')
    print(err_msg)
    sys.exit()


class Event:
    def __init__(self, start_timestamp: str, end_timestamp: str, name: str, 
                 emoji: str = None) -> None:
        self.start_time = start_timestamp
        self.end_time = end_timestamp
        self.m_start_time = to_m_time(start_timestamp)
        self.m_end_time = to_m_time(end_timestamp)
        self.name = name
        self.emoji = emoji
    
    def __repr__(self) -> str:
        return "{}{}".format(
            self.name, 
            self.emoji if self.emoji else '')
    
    def spans_midnight(self) -> bool:
        return self.m_start_time > self.m_end_time
    
    def minutes_left(self, m_time: int) -> int:
        if self.spans_midnight():
            if m_time >= self.m_start_time:
                return to_m_time("24:00") - m_time + self.m_end_time
            else:
                return self.m_end_time - m_time
        else:
            return self.m_end_time - m_time
        
    def is_ongoing(self, m_time: int) -> bool:
        if self.spans_midnight():
            if m_time >= self.m_start_time or m_time < self.m_end_time:
                return True
        elif self.m_start_time <= m_time < self.m_end_time:
            return True
        return False


def load_timetable(csv_file) -> List[Event]:
    """Load timetable from CSV file as a list of Events.
    
    Each line of the CSV file is in the format of
    ```
    <start_time>,<end_time>,<event_name>[,<event_emoji>]
    ```    
    The event emoji column is optional.
    """
    if csv_file is None or not os.path.exists(csv_file):
        exit_with_error(
            f'Invalid timetable file path: {csv_file}\n'
            'Please configure the absolute path to your CSV file in xbar.\n'
            'If you are on SwiftBar or other platforms w/o user-configurable variables,\n'
            'modify the plugin script yourself to specify the path as the variable CSV_TIMETAB.')
    
    tab = []
    with open(csv_file, newline='') as file:
        reader = csv.reader(file)
        for idx, ln in enumerate(reader):
            if len(ln) not in (3, 4):
                exit_with_error(
                    'Each line of the CSV file needs to have 3 or 4 columns,\n'
                    f'but line:{idx} (start from 0) in the given file has {len(ln)} columns.')
            if len(ln) == 3:
                ln.append(None)
            elif ln[3] == '':
                ln[3] = None
                
            start_time, end_time, event_name, event_emoji = ln
            tab.append(Event(start_time, end_time, event_name, emoji=event_emoji))
    return tab


def locate_event(cur_m_time: int, timetable: List[Event]) -> int:
    """Locate the current event.
    
    Returns:
        int: The index of current event. Return -1 if it's not found.
    """
    for idx, event in enumerate(table):
        if event.is_ongoing(cur_m_time):
            return idx
    return -1


def get_time_left_str(m_left: int, mod='') -> str:
    hr_left = m_left / 60
    if hr_left >= 1:
        t = f"{hr_left:.1f}h"
    else:
        t = f"{m_left}m"
    
    if 'l' in mod:
        t += " left"
    if 'p' in mod:
        t = f"({t})"   
    if 's' in mod:
        t = ' ' + t
    return t


if __name__ == "__main__":
    table = load_timetable(CSV_TIMETAB)
    cur_time = datetime.datetime.now().strftime("%H:%M")
    cur_m_time = to_m_time(cur_time)
    
    idx_found = locate_event(cur_m_time, table)
    if idx_found != -1: # display the first-hit ongoing event as title
        event = table[idx_found]
        
        def adaptive_time_left_str():
            m_left = event.minutes_left(cur_m_time)
            if event.emoji:
                return get_time_left_str(m_left)
            else:
                return get_time_left_str(m_left, mod='ps')
            
        print('{} {}'.format(
            event.emoji if DISP_EMJ and event.emoji else event.name,
            adaptive_time_left_str() if DISP_TIME_LEFT else ""
        ))
    else:
        print("no event")
    
    print("---")
    for cur_idx, event in enumerate(table):
        if event.is_ongoing(cur_m_time): # allows multiple ongoing events
            m_left = event.minutes_left(cur_m_time)
            print("{}-{}  {}{}| font={} size={} color={}".format(
                event.start_time, event.end_time, 
                event,
                get_time_left_str(m_left, mod='pls') if not DISP_TIME_LEFT else "",
                FONT, FONT_SIZ, RGB_ORANGE
            ))
        else:
            print("{}-{}  {}| font={} size={} color={}".format(
                event.start_time, event.end_time, event.name, 
                FONT, FONT_SIZ, RGB_LIGHT_GREY
            ))
