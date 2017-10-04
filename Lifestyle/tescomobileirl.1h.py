#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Tesco Mobile Ireland Balance</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>Jack Higgins</bitbar.author>
# <bitbar.author.github>skhg</bitbar.author.github>
# <bitbar.desc>Displays your current Tesco Mobile Ireland balance and available data, text and voice allowances</bitbar.desc>
# <bitbar.image>https://github.com/skhg/BitBar-Plugins/blob/master/TescoMobileIrl/tescobalance.jpg?raw=true</bitbar.image>
# <bitbar.dependencies>python 2.7 or 3.6, pytescomobileirl</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/skhg/BitBar-Plugins/tree/master/TescoMobileIrl</bitbar.abouturl>







# START USER DETAILS

# Enter your tescomobile.ie login details here. These are sent only to the tesco mobile
# website and never stored or transmitted anywhere else.

phone_num = "MyPhoneNumber"
password = "MyPassword"

# END USER DETAILS






from sys import exit

# VERIFY DEPENDENCIES
try:
    from pytescomobileirl import TescoSession
except ImportError:
    print("Tesco Mobile Ireland")
    print("---")
    print("Looks like the package 'pytescomobileirl' isn't installed.")
    print("You need it to run this tool. To install, click 'Install Now',")
    print("then click 'Preferences' -> 'Refresh All...'")
    print("Install Now. | bash='sudo /usr/local/bin/pip install pytescomobileirl'")
    exit()






# START APP

import pickle
import os
import subprocess

class StateMgmt:
    
    def __init__(self):
        os.chdir(self.get_bitbar_plugins_dir())
        self.relative_state_dir = "./.tescomobileirl_state/"
        self.state_dump_file = self.relative_state_dir+"tescomobileirl_last_state.pickle"

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
    
    def dump_state(self, current_state):
        self.check_state_dir_exists(self.relative_state_dir)

        with open(self.state_dump_file,"w") as f_write:
            pickle.dump(current_state,f_write)

class ResultsFormatter:
	def print_output(self, balances, is_live):
		print(balances.remaining_total("data").summary())
		print("---")
		if is_live is False:
		    print("❌ : Using cached data, last update failed")
		    print("---")

		print("Number: "+phone_num)
		print("---")
		print(u'€{:,.2f} credit'.format(balances.credit_remaining).encode("utf-8"))
		print("---")
		print("Active bundles:")
		for bundle in balances.active_balances():
		    print(bundle.summary() + " for "+ str(bundle.days_remaining())+" days")
		print("---")

		print("tescomobile.ie | href=https://my.tescomobile.ie/tmi-selfcare-web/login")
    
	def print_error_message(self):
		print("❌")
		print("---")
		print("Error: Unable to retrieve Tesco Mobile state.")
		print("---")

		print("tescomobile.ie | href=https://my.tescomobile.ie/tmi-selfcare-web/login")

def run():
    login_ok= False

    session = TescoSession()

    try:
        login_ok = session.login(phone_num, password)
    except Exception:
        pass

    state = StateMgmt()
    formatter = ResultsFormatter()

    if login_ok:
        balances = session.get_balances()
        
        state.dump_state(balances)
        formatter.print_output(balances,True)
    else:
        loaded_state = state.load_state()
        if loaded_state is not None:
            formatter.print_output(loaded_state,False)
        else:
            formatter.print_error_message()

run()
