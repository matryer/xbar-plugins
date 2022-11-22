#!/opt/local/bin/python
# -*- coding: utf-8 -*-
#
# <xbar.title>Renault Zero Emission</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Antoine Coetsier</xbar.author>
# <xbar.author.github>retrack</xbar.author.github>
# <xbar.desc>Pulls your Renault EV battery status current and estimated</xbar.desc>
# <xbar.image>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</xbar.image>
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
