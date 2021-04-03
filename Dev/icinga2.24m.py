#!/usr/bin/env python
# -*- coding: utf-8 -*- 
#
# <xbar.title>Icinga2 Status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Mario Mann</xbar.author>
# <xbar.author.github>mariomann</xbar.author.github>
# <xbar.desc>Displays the amount of Service with state OK, WARNING, CRITICAL and UNKNOWN</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/mariomann/pictures/master/icinga2-api.png</xbar.image>
# <xbar.dependencies>python, icinga2, icinga2-api</xbar.dependencies>
# <xbar.abouturl>http://docs.icinga.org/icinga2/snapshot/doc/module/icinga2/chapter/icinga2-api</xbar.abouturl>

import requests, json

OK = '\033[92m'
WARNING = '\033[93m'
CRITICAL = '\033[91m'
UNKNOWN='\033[95m'
ENDC = '\033[0m'

class State:
    HOST     = "localhost"
    PORT     = "5665"
    USER     = "root"
    PASSWORD = "icinga"
    msg  = ""
    
    def getHostCount(self, value):
        requests.packages.urllib3.disable_warnings()
        count = "n/a"
        
        try:
            url = "https://" + self.HOST + ":" + self.PORT + "/v1/objects/hosts"
            headers = { 'Accept': 'application/json', 'X-HTTP-Method-Override': 'GET' }
            data = { "attrs": [ "name", "state" ], "filter": "match(\"" + str(value) + "\", host.state)" }
        
            result = requests.post(url, headers=headers, auth=(self.USER, self.PASSWORD), data=json.dumps(data), verify=False)
    
            if (result.status_code == 200):
                data = json.loads(json.dumps(result.json()))
                count = str(len(data['results']))
    
        except Exception, e:
                self.msg = str(e.message)
                
        return count
        
    
    def getServiceCount(self, value):
        requests.packages.urllib3.disable_warnings()
        count = "n/a"
        
        try:
            url = "https://" + self.HOST + ":" + self.PORT + "/v1/objects/services"
            headers = { 'Accept': 'application/json', 'X-HTTP-Method-Override': 'GET' }
            data = { "attrs": [ "name", "state" ], "filter": "match(\"" + str(value) + "\", service.state)" }
        
            result = requests.post(url, headers=headers, auth=(self.USER, self.PASSWORD), data=json.dumps(data), verify=False)

            if (result.status_code == 200):
                data = json.loads(json.dumps(result.json()))
                count = str(len(data['results']))
        except Exception, e:
            self.msg = str(e.message)
            
        return count

    def getMessage(self):
        return self.msg

s = State()
print ('🌐 | size=16')
print '---'
print 'Host UP:\t\t' + s.getHostCount(0)
print "Host DOWN:\t\t" + s.getHostCount(1)
print '---'
print "Service OK:\t\t\t" + s.getServiceCount(0)
print "Service WARNING:\t" + s.getServiceCount(1)
print "Service CRITICAL:\t" + s.getServiceCount(2)
print "Service UNKNOWN:\t" + s.getServiceCount(3)
if (s.getMessage() != ""):
    print '---'
    print s.getMessage()