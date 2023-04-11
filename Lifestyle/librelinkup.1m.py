#!/usr/bin/env python3

# for homebrew installation add '-S PATH="${PATH}:/opt/homebrew/bin/python3"' between /env and python3

# <xbar.title>LibreLinkUp Status</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.author>Florian Schlund</xbar.author>
# <xbar.author.github>FloSchl8</xbar.author.github>
# <xbar.desc>Gets your GCM from LibreLinkUp: https://librelinkup.com/</xbar.desc>
# <xbar.dependencies>python3, python requests</xbar.dependencies>
# <xbar.image>https://i.imgur.com/XUyOT9o.png</xbar.image>
# <xbar.var>string(VAR_MAIL=""): Your e-mail.</xbar.var>
# <xbar.var>string(VAR_PASSWORD=""): Your password.</xbar.var>
# <xbar.var>select(VAR_COUNTRY="eu"): Your region/country. [us, eu, de, fr, jp, ap, au, ae]</xbar.var>
# <xbar.var>string(VAR_FIRST_PATIENT=""): First patient to show</xbar.var>

import requests
import os

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

class Patient:
    def __init__(self, patient_id, first_name, last_name):
        self.patient_id = patient_id
        self.first_name = first_name
        self.last_name = last_name

headers = {
    "version": "4.2.1",
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
        else:
            print("❌ Auth error: " + auth.json()["error"]["message"])


def get_patients(token):

    connection_url = "https://api-" + country + ".libreview.io/llu/connections"

    payload = ""
    headers["Authorization"] = "Bearer " + token

    response = requests.request("GET", connection_url, data=payload, headers=headers)

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
        low_or_high = value >= connection["targetLow"] or value >= connection["targetHigh"]
        return (value, connection["glucoseMeasurement"]["TrendArrow"], low_or_high)


token = get_auth_token()

if (token is not None):
    patients = get_patients(token=token)

    if first_patient != "":
        for i in range(1, len(patients)):
            if first_patient == patients[i].patient_id:
                # switch our wanted first with the actual first
                patients[0], patients[i] = patients[i], patients[0]

    for i in range(len(patients)):
        (value, trend, low_or_high) = get_measurment(token=token, patientId=patients[i].patient_id)
        if trend != 0 and trend != 6:

            prefix = ""
            if i > 0:
                prefix = patients[i].first_name[0] + ". " + patients[i].last_name[0] + ".: "

            trend_arrow = ""
            if trend == 1:
                trend_arrow = "⬇️"
            elif trend == 2:
                trend_arrow = "↘️"
            elif trend == 3:
                trend_arrow = "➡️"
            elif trend == 4:
                trend_arrow = "↗️"
            elif trend == 5:
                trend_arrow = "⬆️"
            
            if low_or_high:
                color = "red"
            else:
                color = "white"

            print(prefix + str(value) + " " + trend_arrow + " | color=" + color)
        else:
            print("❌ error")

    print("---")

    for p in patients:
        print(p.first_name[0] + ". " + p.last_name[0] + ".: " + p.patient_id)
else:
    print("Error getting Auth Token")

