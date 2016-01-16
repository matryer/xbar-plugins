#!/opt/local/bin/python
# -*- coding: utf-8 -*-
#
# <bitbar.title>Renault Zero Emission</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Antoine Coetsier</bitbar.author>
# <bitbar.author.github>retrack</bitbar.author.github>
# <bitbar.desc>Pulls your Renault EV battery status current and estimated</bitbar.desc>
# <bitbar.image>http://i.imgur.com/Wb1IUoO.png</bitbar.image>
#
# Installation:
# 1/ adjust your Python Binary location (embedded, Homebrew or Macports)
# 2/ install request module with "sudo pip install requests"
# 3/ change user and password to your my.renault.com credentials

import requests
import xml.etree.ElementTree as ET

user = "email address"
password = "your password"


headers = {'Content-Type': 'text/xml'}

# forge the authentication payload
xml = """
<ns1:SmartphoneNewLoginRequest xmlns:ns1="urn:com:renault:smartphone.userservices:v1" xmlns:ns3="urn:com:renault:gdc:type:user:v1" xmlns:ns4="urn:com:hitachi:gdc:type:authenticationmanagercommon:v1" xmlns:ns2="urn:com:hitachi:gdc:type:authenticationmanagercommon:v1">
   <SmartphoneNewLoginInfo>
           <ns1:UserNewLoginInfo>
                   <ns4:UserId>{0}</ns4:UserId>
                   <ns4:Password>{1}</ns4:Password>
           </ns1:UserNewLoginInfo>
           <ns1:DeviceToken></ns1:DeviceToken>
           <ns1:UUID></ns1:UUID>
           <ns1:Locale>FR</ns1:Locale>
           <ns1:AppVersion></ns1:AppVersion>
           <ns1:SmartphoneType>IPHONE</ns1:SmartphoneType>
           <CountryCode>FR</CountryCode>
   </SmartphoneNewLoginInfo>
</ns1:SmartphoneNewLoginRequest>
"""

url = "https://rno-smartgtw.viaaq.eu/aqPortal/B2CSmartphoneProxy/"

# perform a user/password authentication
r = requests.post(url + "UserService",data=xml.format(user, password), headers=headers)
session = r.headers.get('Set-Cookie').split(';')[0]
cookie = session.split('=')[1]

# extract VIN number for subsequent requests
root = ET.fromstring(r.content)
for a in root.findall('.//{urn:com:renault:gdc:type:portalcommon:v1}VIN'):
    vin = a.text

# forge the dashboard payload
xml = """
<ns4:SmartphoneGetNewCurrentDataRequest xmlns:ns3="urn:com:renault:gdc:type:portalcommon:v1" xmlns:ns4="urn:com:renault:gdc:type:smartphoneEvDashboard:v1" xmlns:ns2="urn:com:renault:gdc:type:evDashboard:v1">
        <ns3:VehicleServiceRequestHeader>
                <ns3:VIN>{0}</ns3:VIN>
                <ns3:Caller>SMARTPHONE-APP</ns3:Caller>
        </ns3:VehicleServiceRequestHeader>
</ns4:SmartphoneGetNewCurrentDataRequest>
"""

r1 = requests.post(url + "EvDashboardService",data=xml.format(vin), headers=headers, cookies=r.cookies)

root = ET.fromstring(r1.content)
for a in root.findall('.//{urn:com:renault:gdc:type:evDashboard:v1}BatteryRemainingPercent'):
    BatteryRemainingPercent = a.text
    print(BatteryRemainingPercent + " %")
    break
for a in root.findall('.//{urn:com:renault:gdc:type:evDashboard:v1}CruisingRange'):
    CruisingRange = a.text
    print(CruisingRange + " km")
print("---")
print("VIN: " + vin)
