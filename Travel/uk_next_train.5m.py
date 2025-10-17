#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# <xbar.title>UK Next Train</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Marcus Vechiato</xbar.author>
# <xbar.author.github>vechiato</xbar.author.github>
# <xbar.desc>Shows next trains between UK stations with smart commuter mode that switches between to-work and to-home automatically based on time of day. Uses real-time data from National Rail via Huxley2 API (no credentials required).</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/vechiato/uk-next-train/assets/preview.png</xbar.image>
# <xbar.dependencies>python3</xbar.dependencies>
# <xbar.abouturl>https://github.com/vechiato/uk-next-train</xbar.abouturl>

# Variables become preferences in the app:
# <xbar.var>string(VAR_HOME_STATION="WIM"): Your home station code</xbar.var> 
# <xbar.var>string(VAR_WORK_STATION="WAT"): Your work station code</xbar.var>
# <xbar.var>string(VAR_SWITCH_TIME="12:00"): Time to switch from to-work to to-home (24h format, for auto mode)</xbar.var>
# <xbar.var>select(VAR_MODE="auto"): Mode - auto: switches based on time, manual: always home→work</xbar.var>
# <xbar.var>number(VAR_TRAIN_COUNT=3): Number of next trains to show</xbar.var>
# <xbar.var>boolean(VAR_SHOW_PLATFORM=true): Show platform information</xbar.var>
# <xbar.var>boolean(VAR_SHOW_OPERATOR=false): Show train operator</xbar.var>

import os
import sys
import json
import urllib.request
import urllib.error
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple

class UKTrainMonitor:
    """UK Train monitoring for xbar using Huxley2 API."""
    # Station name cache
    _station_name_cache = {}
    
    def __init__(self):
        # Get environment variables (xbar preferences)
        self.home_station = os.getenv('VAR_HOME_STATION', 'WIM')
        self.work_station = os.getenv('VAR_WORK_STATION', 'WAT')
        self.switch_time = os.getenv('VAR_SWITCH_TIME', '12:00')
        self.mode = os.getenv('VAR_MODE', 'auto')
        self.train_count = int(os.getenv('VAR_TRAIN_COUNT', '3'))
        self.show_platform = os.getenv('VAR_SHOW_PLATFORM', 'true').lower() == 'true'
        self.show_operator = os.getenv('VAR_SHOW_OPERATOR', 'false').lower() == 'true'
        
        # Dark mode detection
        self.is_dark_mode = os.getenv('XBARDarkMode', 'false').lower() == 'true'
        
    def get_station_name(self, code: str) -> str:
        """Get full station name from code using Huxley2 API."""
        if not code:
            return 'Unknown'
        
        code_upper = code.upper()
        
        # Check cache first
        if code_upper in self._station_name_cache:
            return self._station_name_cache[code_upper]
        
        try:
            # Use Huxley2 departures endpoint to get station name
            # This returns the locationName field which contains the full station name
            url = f"https://huxley2.azurewebsites.net/departures/{code_upper}/1"
            
            with urllib.request.urlopen(url, timeout=5) as response:
                if response.status == 200:
                    data = json.loads(response.read().decode())
                    
                    # Extract locationName which contains the full station name
                    station_name = data.get('locationName')
                    
                    if station_name:
                        # Cache the result
                        self._station_name_cache[code_upper] = station_name
                        return station_name
        except Exception:
            # If API call fails, fall back to code
            pass
        
        # Fallback to the code itself if lookup fails
        return code_upper
    
    def determine_route(self) -> Tuple[str, str, str]:
        """Determine current route based on mode and time."""
        if self.mode == 'manual':
            # Manual mode: always show home → work
            return self.home_station, self.work_station, "Manual (Home→Work)"
        
        # Auto mode - determine based on time
        current_time = datetime.now().strftime('%H:%M')
        switch_time = self.switch_time
        
        if current_time < switch_time:
            # Before switch time - going to work
            return self.home_station, self.work_station, "To Work"
        else:
            # After switch time - going home
            return self.work_station, self.home_station, "To Home"
    
    def fetch_train_data(self, from_station: str, to_station: str) -> List[Dict]:
        """Fetch train data from Huxley2 API."""
        try:
            # Huxley2 API endpoint
            url = f"https://huxley2.azurewebsites.net/departures/{from_station}/to/{to_station}"
            
            with urllib.request.urlopen(url, timeout=10) as response:
                if response.status == 200:
                    data = json.loads(response.read().decode())
                    return self.parse_train_data(data)
                else:
                    return []
        except Exception as e:
            # Return error info for debugging
            return [{'error': str(e)}]
    
    def parse_train_data(self, data: dict) -> List[Dict]:
        """Parse Huxley API response."""
        trains = []
        
        if 'trainServices' not in data or not data['trainServices']:
            return trains
        
        current_time = datetime.now()
        
        for service in data['trainServices']:
            try:
                # Parse scheduled time
                std = service.get('std', 'Unknown')
                if std == 'Unknown':
                    continue
                    
                sched_time = datetime.strptime(std, '%H:%M').replace(
                    year=current_time.year,
                    month=current_time.month,
                    day=current_time.day
                )
                
                # Handle next day services
                if sched_time < current_time:
                    sched_time = sched_time + timedelta(days=1)
                
                # Only include future trains
                if sched_time >= current_time:
                    etd = service.get('etd', std)
                    destination = self.get_destination_name(service.get('destination', []))
                    platform = service.get('platform') or 'TBC'
                    operator = service.get('operator', 'Unknown')
                    
                    # Determine status
                    status = self.get_service_status(service)
                    
                    # Calculate minutes until departure
                    time_diff = sched_time - current_time
                    minutes_until = int(time_diff.total_seconds() / 60)
                    
                    trains.append({
                        'scheduled': std,
                        'estimated': etd,
                        'destination': destination,
                        'platform': platform,
                        'operator': operator,
                        'status': status,
                        'minutes_until': minutes_until,
                        'is_delayed': self.is_delayed(service),
                        'is_cancelled': service.get('isCancelled', False)
                    })
                    
            except (ValueError, KeyError):
                continue
        
        # Sort by departure time and limit
        trains.sort(key=lambda x: x['minutes_until'])
        return trains[:self.train_count]
    
    def get_destination_name(self, destinations: List[Dict]) -> str:
        """Extract destination name."""
        if destinations and len(destinations) > 0:
            name = destinations[0].get('locationName', 'Unknown')
            # Shorten long station names for display
            if len(name) > 15:
                name = name.replace('London ', '').replace(' Airport', '')
            return name
        return 'Unknown'
    
    def get_service_status(self, service: Dict) -> str:
        """Determine service status."""
        if service.get('isCancelled'):
            return 'Cancelled'
        
        etd = service.get('etd', '')
        std = service.get('std', '')
        
        if etd == 'Delayed':
            return 'Delayed'
        elif etd == 'On time':
            return 'On time'
        elif etd and std and etd != std:
            return f"Exp {etd}"
        else:
            return 'On time'
    
    def is_delayed(self, service: Dict) -> bool:
        """Check if service is delayed."""
        etd = service.get('etd', '')
        std = service.get('std', '')
        return etd not in ['On time', std] and not service.get('isCancelled', False)
    
    def get_status_emoji(self, train: Dict) -> str:
        """Get emoji for train status."""
        if train['is_cancelled']:
            return '❌'
        elif train['is_delayed']:
            return '⏰'
        else:
            return '✅'
    
    def get_menu_bar_color(self, trains: List[Dict]) -> str:
        """
        Get menu bar color based on all trains status:
        - Red: Next train is cancelled
        - Orange: Any train apart from the next is not on time
        - Green: All trains have no issues
        """
        if not trains:
            return 'gray'
        
        # Check if next train is cancelled
        if trains[0]['is_cancelled']:
            return 'red'
        
        # Check if any train (apart from the next) has issues
        for train in trains[1:]:
            if train['is_cancelled'] or train['is_delayed']:
                return 'orange'
        
        # All trains are on time
        return 'green'
    
    def format_time_until(self, minutes: int) -> str:
        """Format time until departure."""
        if minutes <= 0:
            return 'Now'
        elif minutes == 1:
            return '1m'
        elif minutes < 60:
            return f'{minutes}m'
        else:
            hours = minutes // 60
            mins = minutes % 60
            return f'{hours}h{mins}m' if mins > 0 else f'{hours}h'
    
    def generate_output(self):
        """Generate xbar output."""
        from_station, to_station, mode_desc = self.determine_route()
        trains = self.fetch_train_data(from_station, to_station)
        
        # Handle errors
        if trains and 'error' in trains[0]:
            print("🚂❌")
            print("---")
            print(f"Error: {trains[0]['error']} | color=red")
            print(f"Check connection or station codes | color=gray")
            return
        
        # Handle no trains
        if not trains:
            print("🚂❓")
            print("---")
            print(f"No trains: {self.get_station_name(from_station)} → {self.get_station_name(to_station)} | color=orange")
            print(f"Mode: {mode_desc} | color=gray")
            return
        
        # Menu bar display - show next train
        next_train = trains[0]
        time_until = self.format_time_until(next_train['minutes_until'])
        
        # Determine menu bar color based on all trains status
        menu_bar_color = self.get_menu_bar_color(trains)
        
        # Main menu bar item
        if next_train['is_cancelled']:
            print(f"🚂 Next: CANCELLED | color={menu_bar_color}")
        else:
            print(f"🚂 Next: {time_until} | color={menu_bar_color}")
        
        # Dropdown menu
        print("---")
        
        # Header with route info
        from_name = self.get_station_name(from_station)
        to_name = self.get_station_name(to_station)
        print(f"🚂 {from_name} → {to_name}")
        print(f"📍 Mode: {mode_desc} | color=gray")
        print(f"🕐 Updated: {datetime.now().strftime('%H:%M:%S')} | color=gray")
        print("---")
        
        # Train list
        for i, train in enumerate(trains):
            status_emoji = self.get_status_emoji(train)
            time_until = self.format_time_until(train['minutes_until'])
            
            # Build train info line
            line_parts = [
                f"{status_emoji} {train['scheduled']}",
                f"({time_until})"
            ]
            
            if train['is_cancelled']:
                line_parts.append("CANCELLED")
                color = "red"
            elif train['is_delayed']:
                line_parts.append(f"→ {train['estimated']}")
                color = "orange"
            else:
                color = "green" if not self.is_dark_mode else "lightgreen"
            
            if self.show_platform and train['platform'] != 'TBC':
                line_parts.append(f"Plat {train['platform']}")
            
            if self.show_operator:
                line_parts.append(train['operator'])
            
            # Format destination
            dest_info = f" to {train['destination']}"
            
            print(f"{' '.join(line_parts)}{dest_info} | color={color} font=Monaco")
        
        # Footer actions
        print("---")
        print(f"🔄 Refresh | refresh=true")
        
        # Quick switch options in auto mode
        if self.mode == 'auto':
            current_time = datetime.now().strftime('%H:%M')
            if current_time < self.switch_time:
                other_from, other_to = self.work_station, self.home_station
                other_desc = "To Home"
            else:
                other_from, other_to = self.home_station, self.work_station
                other_desc = "To Work"
            
            other_from_name = self.get_station_name(other_from)
            other_to_name = self.get_station_name(other_to)
            print(f"🔄 Show {other_desc} ({other_from_name}→{other_to_name}) | shell='{sys.argv[0]}' param1=quick param2={other_from} param3={other_to} terminal=false refresh=true")
        
        # Settings and info
        print("--⚙️ Settings")
        print(f"--Current: {from_name} → {to_name} | color=gray")
        print(f"--Mode: {self.mode} | color=gray")
        if self.mode == 'auto':
            print(f"--Switch time: {self.switch_time} | color=gray")
        print(f"--Train count: {self.train_count} | color=gray")
        print("--Configure... | href=xbar://app.xbarapp.com/openPlugin?path=" + os.path.abspath(__file__))
        
        # Links
        print("--📱 About")
        print("--GitHub Repository | href=https://github.com/vechiato/uk-next-train")
        print("--National Rail | href=https://www.nationalrail.co.uk/")
        print("--Huxley2 API | href=https://huxley2.azurewebsites.net/")

def main():
    """Main entry point."""
    # Handle quick switch from menu
    if len(sys.argv) >= 4 and sys.argv[1] == 'quick':
        # Temporarily override stations for quick view
        os.environ['VAR_HOME_STATION'] = sys.argv[2]
        os.environ['VAR_WORK_STATION'] = sys.argv[3]
        os.environ['VAR_MODE'] = 'manual'
    
    monitor = UKTrainMonitor()
    monitor.generate_output()

if __name__ == '__main__':
    main()