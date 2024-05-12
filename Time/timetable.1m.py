#!/usr/bin/python3

# xbar meta
# <xbar.title>Timetable</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Cabin Zhu</xbar.author>
# <xbar.author.github>cabinz</xbar.author.github>
# <xbar.desc>Display the current event from a given daily timetable. Make sure to configure the path to your CSV timetable file as the global variable CSV_TIMETAB.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/cabinz/my-xbar-plugins/main/timetable/timetable.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>

import datetime
import csv
from dataclasses import dataclass
from typing import List

# Each line in the format "Begin_Time,End_Time,Event_Name"
#
# Example (timetable.csv):
# 23:30,07:30,sleep
# 07:30,08:30,breakfast
# 08:30,10:00,gym
# ...
CSV_TIMETAB = "/Applications/SwiftBar/.timetable.csv"


def to_minutes(time_str):
    """Convert HH:MM time to minutes since 00:00."""
    hour, minute = map(int, time_str.split(':'))
    return hour * 60 + minute


@dataclass
class Event:
    start_time: str
    end_time: str
    m_start_time: int
    m_end_time: int
    name: str
    
    def __repr__(self) -> str:
        return self.name
    
    def spans_midnight(self) -> bool:
        return self.m_start_time > self.m_end_time


def load_timetable(csv_file) -> List[Event]:
    tab = []
    with open(csv_file, newline='') as file:
        reader = csv.reader(file)
        for idx, (start_time, end_time, event_name) in enumerate(reader):
            tab.append(Event(
                start_time, end_time, 
                to_minutes(start_time), to_minutes(end_time),
                event_name))
    return tab


def locate_event(cur_time: str, timetable: List[Event]) -> int:
    """Locate the current event.

    Args:
        cur_time (str): current timestamp in "%H:%M"

    Returns:
        int: The index of current event. Return -1 if it's not found.
    """
    # Convert current time to minutes since midnight for easier comparison
    cur_m_time = to_minutes(cur_time)
    
    for idx, event in enumerate(table):
        if event.spans_midnight():
            if cur_m_time >= event.m_start_time or cur_m_time < event.m_end_time:
                return idx
        elif event.m_start_time <= cur_m_time < event.m_end_time:
            return idx
    if not event_found:
        return -1


if __name__ == "__main__":
    table = load_timetable(CSV_TIMETAB)
    cur_time = datetime.datetime.now().strftime("%H:%M")
    
    idx_found = locate_event(cur_time, table)
    event_found = (idx_found != -1)
    print(table[idx_found] if event_found else "no event")
    
    print("---")
    for cur_idx, event in enumerate(table):
        is_cur_event = event_found and idx_found == cur_idx
        print("{}-{} {}{} | font=Monaco size=15 color={}".format(
            event.start_time, event.end_time, event.name,
            " ⬅" if is_cur_event else "",
            "orange" if is_cur_event else "light_color" 
        ))
