#!/usr/bin/env python3

# for homebrew installation add '-S PATH="${PATH}:/opt/homebrew/bin/python3"' between /env and python3

# <xbar.title>LibreLinkUp Status</xbar.title>
# <xbar.version>v1.2</xbar.version>
# <xbar.author>Florian Schlund</xbar.author>
# <xbar.author.github>FloSchl8</xbar.author.github>
# <xbar.desc>Gets your GCM from LibreLinkUp: https://librelinkup.com/</xbar.desc>
# <xbar.dependencies>python3, python requests</xbar.dependencies>
# <xbar.image>https://i.imgur.com/XUyOT9o.png</xbar.image>
# <xbar.var>string(VAR_MAIL=""): Your e-mail.</xbar.var>
# <xbar.var>string(VAR_PASSWORD=""): Your password.</xbar.var>
# <xbar.var>select(VAR_COUNTRY="eu"): Your region/country. [us, eu, de, fr, jp, ap, au, ae]</xbar.var>
# <xbar.var>string(VAR_FIRST_PATIENT=""): First patient to show (optional)</xbar.var>

import os
from datetime import datetime

# your LibreLibkUp login (this is NOT your LibreView Login)
email = os.environ.get("VAR_MAIL")
password = os.environ.get("VAR_PASSWORD")

# your region/country
# available: us, eu, de, fr, jp, ap, au, ae
country = os.environ.get("VAR_COUNTRY")

# used for ordering multiple patients
# the value from this id will be displayed in the menu bar first, all others can be seen in the dropdown
# all IDs will also be shown after the glucose values
# if you have only one patient you can leave this blank
first_patient = os.environ.get("VAR_FIRST_PATIENT")

excessive_time_color = "fuchsia"
error_color = None

min_seconds_to_show_excessive_time_color = 60 + 15 # 1min 15s
max_seconds_to_display_data = 60*60*8 # 8h

use_custom_range = True
custom_range_high = 185
custom_range_slightly_high = 145
custom_range_low = 65

def printError(error_message: str, error: Exception):
    print("Error | " + makeColorString(error_color))
    print("---")
    print("Error: " + error_message)
    if error is not None: print(error)

try:
    import requests
except ImportError:
    printError("Requests module not found. Install it by running 'pip install requests' in the terminal.")

class Patient:
    def __init__(self, patient_id, first_name, last_name):
        self.patient_id = patient_id
        self.first_name = first_name
        self.last_name = last_name

headers = {
    "version": "4.7.0",
    "product": "llu.android",
    "Connection": "keep-alive",
    "Pragma": "no-cache",
    "Cache-Control": "no-cache",
    "Content-Type": "application/json"
    }

def get_auth_token():
    authurl = "https://api-" + country + ".libreview.io/llu/auth/login"

    payload = {
    "email": email,
    "password": password
    }
    

    auth = requests.request("POST", authurl, json=payload, headers=headers)
    if auth.ok:
        if auth.json()["status"] == 0:
            return auth.json()["data"]["authTicket"]["token"]
        elif auth.json()["status"] == 4:
            raise Exception("Check Terms Of Service agreement")
        else:
            raise Exception("Auth error: " + auth.json()["error"]["message"])

def get_patients(token):
    connection_url = "https://api-" + country + ".libreview.io/llu/connections"

    payload = ""
    headers["Authorization"] = "Bearer " + token

    response = requests.request("GET", connection_url, data=payload, headers=headers)
    response.raise_for_status()

    if response.ok:
        ids = []
        for d in response.json()["data"]:
            ids.append(Patient(d["patientId"], d["firstName"], d["lastName"]))
        return ids

def get_measurment(token, patientId):
    url = "https://api-" + country + ".libreview.io/llu/connections/" + patientId + "/graph"

    payload = ""
    headers["Authorization"] = "Bearer " + token

    response = requests.request("GET", url, data=payload, headers=headers)
    if response.ok:
        connection = response.json()["data"]["connection"]
        value = connection["glucoseMeasurement"]["Value"]
        patient_range_high = connection["targetHigh"]
        patient_range_low = connection["targetLow"]
        timestamp_string = connection["glucoseMeasurement"]["Timestamp"]
        timestamp = datetime.strptime(timestamp_string, "%m/%d/%Y %I:%M:%S %p")
        return (value, connection["glucoseMeasurement"]["TrendArrow"], patient_range_high, patient_range_low, timestamp)

def get_prefix(patient: Patient):
    return patient.first_name[0] + ". " + patient.last_name[0] + ".: "

def time_diff_to_string(seconds_ago: int):
    if seconds_ago < 60:
        return str(seconds_ago) + "s"
    if seconds_ago < 60*60:
        return str(seconds_ago // 60) + "min"
    if seconds_ago < 60*60*24:
        return str(seconds_ago // (60*60)) + "h"
    if seconds_ago < 60*60*24*2:
        return str(seconds_ago // (60*60*24)) + "day"
    return str(seconds_ago // (60*60*24)) + "days"

def get_color_from_range(value: int, patient_range_low: int, patient_range_high: int):
    if use_custom_range:
        if value > custom_range_high:
            return "red"
        if value > custom_range_slightly_high:
            return "yellow"
        if value < custom_range_low:
            return "red"
    else:
        if value > patient_range_high:
            return "red"
        if value < patient_range_low:
            return "red"
    return None

def get_trend_arrow(trend: int):
    return {
        1: "↓",
        2: "↘",
        3: "→",
        4: "↗",
        5: "↑"
    }.get(trend, "")

def makeColorString(color):
    return "color=" + color if color is not None else ""    

def main():
    try:
        token = get_auth_token()
        if (token is None): raise Exception()
    except Exception as e:
        token = None
        printError("Error getting auth token\nCheck your internet connection or username and password", e)

    if(token is not None):
        try:
            patients = get_patients(token=token)
            if (patients is None): raise Exception()
        except Exception as e:
            patients = None
            printError("Error getting patients", e)
        
        if(patients is not None):
            if first_patient != "":
                for i in range(1, len(patients)):
                    if first_patient == patients[i].patient_id:
                        # switch our wanted first with the actual first
                        patients[0], patients[i] = patients[i], patients[0]

            for i in range(len(patients)):
                (
                    value, 
                    trend, 
                    patient_range_high, 
                    patient_range_low, 
                    timestamp
                ) = get_measurment(token=token, patientId=patients[i].patient_id)
                prefix = get_prefix(patients[i]) if i else ""
                trend_arrow = get_trend_arrow(trend)
                color = get_color_from_range(value, patient_range_low, patient_range_high)
                seconds_ago = (datetime.now() - timestamp).seconds
                if seconds_ago >= min_seconds_to_show_excessive_time_color and excessive_time_color is not None:
                    color = excessive_time_color

                if seconds_ago > max_seconds_to_display_data:
                    print("No data | " + makeColorString(error_color))
                else:
                    print(prefix + str(value) + trend_arrow + " | " + makeColorString(color))

            if len(patients) > 1:
                print("---")
                for p in patients:
                    print(get_prefix(p) + p.patient_id)

    print("---")
    print("Last update: " + datetime.now().strftime('%H:%M:%S %Y-%m-%d'))
    is_data_valid = token is not None and patients is not None
    if is_data_valid and len(patients) == 1:
        print("Last measurement: " + time_diff_to_string(seconds_ago) + " ago  (since update)")
    print("Refresh | href="+ "xbar://app.xbarapp.com/refreshPlugin?path={}".format(os.path.basename(__file__)))

main()
