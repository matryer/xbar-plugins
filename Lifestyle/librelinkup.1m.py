#!/usr/bin/env python3

# for homebrew installation add '-S PATH="${PATH}:/opt/homebrew/bin/python3"' between /env and python3

# <xbar.title>LibreLinkUp Status</xbar.title>
# <xbar.version>v2.1</xbar.version>
# <xbar.author>Florian Schlund,Maurici Abad</xbar.author>
# <xbar.author.github>FloSchl8,mauriciabad</xbar.author.github>
# <xbar.desc>Display your blood glucose readings and it's trend. The data comes from LibreLinkUp's API: https://librelinkup.com/ so you must have a compatible CGM (any Freestyle Libre), and a user account connected to your main device. Other keywords: Diabetes, blood sugar, monitor values or readings.</xbar.desc>
# <xbar.dependencies>python3,python requests</xbar.dependencies>
# <xbar.image>https://i.imgur.com/RATfZs3.png</xbar.image>
#
# <xbar.var>string(VAR_MAIL=""): Your LibreLinkUp e-mail.</xbar.var>
# <xbar.var>string(VAR_PASSWORD=""): Your LibreLinkUp password.</xbar.var>
# <xbar.var>select(VAR_COUNTRY="eu"): Your region/country. [us, eu, eu2, de, fr, jp, ap, au, ae, ca, worldwide]</xbar.var>
# <xbar.var>string(VAR_FIRST_PATIENT_ID=""): (optional) Id of the first patient to show. It is useful when you have multiple people linked to the same LibreLinkUp account.</xbar.var>
# <xbar.var>number(VAR_MAX_SECONDS_TO_DISPLAY_DATA=28800): (optional) Measurements older than this won't be shown. (in seconds, default 8h)</xbar.var>
# <xbar.var>number(VAR_MIN_SECONDS_TO_SHOW_EXCESSIVE_TIME_COLOR=75): (optional) Measurements older than this will be shown with a different color (Excessive time color).</xbar.var>
# <xbar.var>string(VAR_EXCESSIVE_TIME_COLOR="fuchsia"): (optional) Color to display when the last measurement is too old.</xbar.var>
# <xbar.var>string(VAR_ERROR_COLOR=""): (optional) Color to display when an error occurs.</xbar.var>
# <xbar.var>number(VAR_CUSTOM_RANGE_HIGH=185): (optional) Custom high threshold for the range.</xbar.var>
# <xbar.var>number(VAR_CUSTOM_RANGE_SLIGHTLY_HIGH=145): (optional) Custom slightly high threshold for the range.</xbar.var>
# <xbar.var>number(VAR_CUSTOM_RANGE_LOW=65): (optional) Custom low threshold for the range.</xbar.var>
# <xbar.var>string(VAR_RANGE_COLOR_HIGH="red"): (optional) Color to display when the value is above the high threshold.</xbar.var>
# <xbar.var>string(VAR_RANGE_COLOR_SLIGHTLY_HIGH="yellow"): (optional) Color to display when the value is above the slightly high threshold and below high threshold.</xbar.var>
# <xbar.var>string(VAR_RANGE_COLOR_LOW="red"): (optional) Color to display when the value is below the low threshold.</xbar.var>

import hashlib
import os
from datetime import datetime

# your LibreLibkUp login (this is NOT your LibreView Login)
email = os.environ.get("VAR_MAIL")
password = os.environ.get("VAR_PASSWORD")

# your region/country
# available: us, eu, eu2, de, fr, jp, ap, au, ae, ca, worldwide
country = os.environ.get("VAR_COUNTRY")

# used for ordering multiple patients
# the value from this id will be displayed in the menu bar first, all others can be seen in the dropdown
# all IDs will also be shown after the glucose values
# if you have only one patient you can leave this blank
first_patient_id = os.environ.get("VAR_FIRST_PATIENT_ID")

excessive_time_color = os.environ.get("VAR_EXCESSIVE_TIME_COLOR")
error_color = os.environ.get("VAR_ERROR_COLOR")

min_seconds_to_show_excessive_time_color = int(os.environ.get("VAR_MIN_SECONDS_TO_SHOW_EXCESSIVE_TIME_COLOR"))
max_seconds_to_display_data = int(os.environ.get("VAR_MAX_SECONDS_TO_DISPLAY_DATA"))

custom_range_high = int(os.environ.get("VAR_CUSTOM_RANGE_HIGH"))
custom_range_slightly_high = int(os.environ.get("VAR_CUSTOM_RANGE_SLIGHTLY_HIGH"))
custom_range_low = int(os.environ.get("VAR_CUSTOM_RANGE_LOW"))
range_color_high = os.environ.get("VAR_RANGE_COLOR_HIGH")
range_color_slightly_high = os.environ.get("VAR_RANGE_COLOR_SLIGHTLY_HIGH")
range_color_low = os.environ.get("VAR_RANGE_COLOR_LOW")

def printError(error_message: str, error: Exception):
    print("Error | " + makeColorString(error_color))
    print("---")
    print("Error: " + error_message)
    if error is not None: print(error)

try:
    import requests
except ImportError as e:
    printError("Requests module not found. Install it by running 'pip install requests' in the terminal.", e)

class Patient:
    def __init__(self, patient_id, first_name, last_name):
        self.patient_id = patient_id
        self.first_name = first_name
        self.last_name = last_name

def get_base_headers():
    return {
        "accept-encoding": "gzip",
        "cache-control": "no-cache",
        "connection": "Keep-Alive",
        "content-type": "application/json",
        "product": "llu.ios",
        "version": "4.16.0"
    }

def calculate_sha256(input_string):
    sha256_hash = hashlib.sha256()
    sha256_hash.update(input_string.encode())
    hex_hash = sha256_hash.hexdigest()
    return hex_hash

def get_auth_token():
    global country
    authurl = f"https://api-{country}.libreview.io/llu/auth/login"

    payload = {
        "email": email,
        "password": password
    }
    
    headers = get_base_headers()
    auth = requests.request("POST", authurl, json=payload, headers=headers)
    if auth.ok:
        auth_data = auth.json()
        if auth_data.get("status") == 0:
            data = auth_data.get("data", {})
            
            # Check if API is requesting a region redirect
            if data.get("redirect") and data.get("region"):
                correct_region = data["region"]
                country = correct_region
                authurl = f"https://api-{country}.libreview.io/llu/auth/login"
                headers = get_base_headers()
                auth = requests.request("POST", authurl, json=payload, headers=headers)
                if auth.ok:
                    auth_data = auth.json()
                    data = auth_data.get("data", {})
                else:
                    raise Exception(f"HTTP {auth.status_code}: {auth.text}")
            
            # Now get the token
            if "authTicket" in data:
                token = data["authTicket"]["token"]
                user_id = data["user"]["id"]
                return token, user_id
            else:
                raise Exception("No authTicket in response")
                
        elif auth_data.get("status") == 4:
            raise Exception("Check Terms Of Service agreement")
        else:
            error_msg = auth_data.get("error", {}).get("message", "Unknown error")
            raise Exception("Auth error: " + error_msg)
    else:
        raise Exception(f"HTTP {auth.status_code}: {auth.text}")

def get_patients(token, user_id):
    connection_url = f"https://api-{country}.libreview.io/llu/connections"

    hex_user_id = calculate_sha256(input_string=user_id)

    headers = get_base_headers()
    headers["Authorization"] = f"Bearer {token}"
    headers["Account-Id"] = hex_user_id

    #print(headers)
    #print(connection_url)
    #print(payload)

    response = requests.request("GET", connection_url, headers=headers)
    response.raise_for_status()

    if response.ok:
        ids = []
        for d in response.json()["data"]:
            ids.append(Patient(d["patientId"], d["firstName"], d["lastName"]))
        return ids

def get_measurment(token, user_id, patientId):
    url = f"https://api-{country}.libreview.io/llu/connections/{patientId}/graph"

    hex_user_id = calculate_sha256(input_string=user_id)
    
    headers = get_base_headers()
    headers["Authorization"] = f"Bearer {token}"
    headers["Account-Id"] = hex_user_id

    response = requests.request("GET", url, headers=headers)
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
    if custom_range_high:
        if value > custom_range_high:
            return range_color_high
    else:
        if value > patient_range_high:
            return range_color_high
    
    if custom_range_slightly_high:
        if value > custom_range_slightly_high:
            return range_color_slightly_high
    
    if custom_range_low:
        if value < custom_range_low:
            return range_color_low
    else:
        if value < patient_range_low:
            return range_color_low

    return None

def get_trend_arrow(trend: int):
    return {
        1: "↓",
        2: "↘",
        3: "→",
        4: "↗",
        5: "↑"
    }.get(trend, "")

def makeColorString(color: str):
    return "color=" + color if color else ""    

def main():
    try:
        token, user_id = get_auth_token()
        if (token is None): raise Exception()
    except Exception as e:
        token = None
        printError("Error getting auth token\nCheck your internet connection or username and password", e)

    if(token is not None):
        try:
            patients = get_patients(token=token, user_id=user_id)
            if (patients is None): raise Exception()
        except Exception as e:
            patients = None
            printError("Error getting patients", e)
        
        if(patients is not None):
            if first_patient_id != "":
                for i in range(1, len(patients)):
                    if first_patient_id == patients[i].patient_id:
                        # switch our wanted first with the actual first
                        patients[0], patients[i] = patients[i], patients[0]

            for i in range(len(patients)):
                (
                    value,
                    trend,
                    patient_range_high,
                    patient_range_low,
                    timestamp
                ) = get_measurment(token=token, user_id=user_id, patientId=patients[i].patient_id)
                prefix = get_prefix(patients[i]) if i else ""
                trend_arrow = get_trend_arrow(trend)
                color = get_color_from_range(value, patient_range_low, patient_range_high)
                seconds_ago = (datetime.now() - timestamp).seconds
                if seconds_ago >= min_seconds_to_show_excessive_time_color and excessive_time_color:
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
