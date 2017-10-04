#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Leap Card Balance</bitbar.title>
# <bitbar.version>v1.2.0</bitbar.version>
# <bitbar.author>Jack Higgins</bitbar.author>
# <bitbar.author.github>skhg</bitbar.author.github>
# <bitbar.desc>Displays your current Leap Card balance along with any recent card events.</bitbar.desc>
# <bitbar.image>https://github.com/skhg/BitBar-Plugins/blob/master/LeapCard/leapcard.jpg?raw=true</bitbar.image>
# <bitbar.dependencies>python 2 or 3, pyleapcard, lxml</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/skhg/BitBar-Plugins/tree/master/LeapCard</bitbar.abouturl>







# START USER DETAILS

# Enter your leapcard.ie login details here. These are sent only to the leap card
# website and never stored or transmitted anywhere else.

leap_user = "MyUserName"
leap_pass = "MyPassword"

# END USER DETAILS





from sys import exit

# VERIFY DEPENDENCIES
try:
    from pyleapcard import LeapSession
except ImportError:
    print("Leap Card")
    print("---")
    print("Looks like the package 'pyleapcard' isn't installed.")
    print("You need it to run this tool. To install, click 'Install Now',")
    print("then click 'Preferences' -> 'Refresh All...'")
    print("Install Now. | bash='sudo /usr/local/bin/pip install pyleapcard'")
    exit()



# START APP

import pickle
import os
import subprocess
import sys

class StateMgmt:
    
    def __init__(self):
        os.chdir(self.get_bitbar_plugins_dir())
        self.relative_state_dir = "./.leapcard_state/"
        self.state_dump_file = self.relative_state_dir+"leapcard_last_state.pickle"

    def get_bitbar_plugins_dir(self):
        bitbar_defaults = subprocess.check_output(["defaults", "read", "com.matryer.BitBar"]).split(";")
        for entry in bitbar_defaults:
            if "pluginsDirectory" in entry:
                return entry.split("\"")[1]

        raise IOError("BitBar plugins directory could not be found")
    
    def check_state_dir_exists(self, state_dir):
        if os.path.exists(state_dir) is False:
            os.mkdir(state_dir)
    
    def load_state(self):
        self.check_state_dir_exists(self.relative_state_dir)

        if os.path.exists(self.state_dump_file) is False:
            return None
        else:
            try:
                with open(self.state_dump_file,"r") as f_read:
                    return pickle.load(f_read)
            except:
                return None
    
    def dump_state(self, card_state,events_state):
        self.check_state_dir_exists(self.relative_state_dir)

        current_state = [card_state,events_state]

        with open(self.state_dump_file,"w") as f_write:
            pickle.dump(current_state,f_write)

class ResultsFormatter:
    
    def euro_value_to_str(self, value, highlightNegative=False):
        balance_string =u""
        negative_balance = False

        if value < 0:
            balance_string = u"- "
            negative_balance = True

        balance_string += u'€{:,.2f}'.format(abs(value))

        if negative_balance and highlightNegative:
            balance_string += " | color=orange"

        return balance_string
    
    def format_card_event(self, event):
        styleInfo = " | font=Courier"

        if event.was_topup is True:
            styleInfo += " color=green"

        euroVal = self.euro_value_to_str(event.price).encode("utf-8")
        start = event.date + " " + event.time + " (" + event.provider + ") "

        return start.encode("utf-8") + euroVal + styleInfo.encode("utf-8")
    
    def print_output(self, card, events, is_live, login_url):
        print(self.euro_value_to_str(card.balance,True).encode("utf-8"))
        print("---")
        if is_live is False:
            print("❌ : Using cached data, last update failed")
            print("---")

        print(card.card_num+" (" + card.card_label + ")".encode())
        print("---")
        print("Recent Events")
        for e in events:
            print(self.format_card_event(e))
        print("---")

        print("leapcard.ie | href="+login_url)
    
    def print_error_message(self, details, login_url):
        print("❌")
        print("---")
        print("Error: Unable to retrieve Leap Card state.")
        print("---")

        print("leapcard.ie | href="+login_url)

def run():
    login_ok= False
    login_error = ""

    session = LeapSession()

    try:
        login_ok = session.try_login(leap_user, leap_pass)
    except Exception:
        login_error = sys.exc_info()[0]

    state = StateMgmt()
    formatter = ResultsFormatter()

    if login_ok:
        card = session.get_card_overview()
        events = session.get_events()
        
        state.dump_state(card,events)
        formatter.print_output(card,events,True, session.login_url())
    else:
        loaded_state = state.load_state()
        if loaded_state is not None:
            formatter.print_output(loaded_state[0],loaded_state[1],False, session.login_url())
        else:
            formatter.print_error_message(login_error, session.login_url())

run()
