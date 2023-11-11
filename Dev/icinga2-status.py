#!/usr/bin/env LC_ALL=de_DE.UTF-8 /opt/local/bin/python3
# -*- coding: utf-8 -*-
# Metadata allows your plugin to show up in the app, and website.
#
# <xbar.title>Icinga Status</xbar.title>
# <xbar.version>v0.9</xbar.version>
# <xbar.author>Sandro Wefel</xbar.author>
# <xbar.author.github>sawh</xbar.author.github>
# <xbar.desc>Show icinga2 tactical overview, the amount of hosts and service in different states and further details</xbar.desc>
# <xbar.image>https://github.com/sawh/xbar-plugins/blob/main/img/CleanShot_xbar1.png?raw=true</xbar.image>
# <xbar.dependencies>python3, python3-json, python3-urllib, python3-pathlib, icinga2-api</xbar.dependencies>
# <xbar.abouturl>https://github.com/sawh/xbar-plugins/</xbar.abouturl>
#
# Work based on Bitbar plugins from
# Arkii https://github.com/arkii/gadgets/blob/master/Utility/BitBar/icinga2-api-overview.py
# <bitbar.author.github>arkii</bitbar.author.github>
# and Thilo Wening <bitbar.author.github>mkayontour</bitbar.author.github>
#
# Limitation: works with acknowledgements for critical services but not for warnings

# For additional information see https://github.com/sawh/xbar-plugins

## Setup

# you need an icinga api user with the following permissions
#object ApiUser "bitbar" {
#  password = "xxxxx"
#  permissions = [ "status/query", "objects/query/*" ]
#}

# Create a config file $HOME/.config/icinga2statuscfg.json with one or more icinga servers or with
# one server and different filters.
# Remove the "<- ..." in the example
#{
#  "SERVER": [
#    {
#        "SERVERDISPLAYNAME": "Sensor - Server XXX", <- name in pulldown menu
#        "SERVER1":"intern.server1.example.com",
#        "SERVER2":"server1.example.com", <- alternate server name or IP if necessare, e.g. for VPN
#        "PORT1":443,
#        "PORT2":5665,            <- port for alternate server
#        "APIURL": "https://{server}:{port}", <- API-URL (use python format with SERVER PORT from above)
#        "BASEURL": "https://{server}:{port}/icingaweb2/monitoring", <- baseURL for links
#        "USER":"bitbar",
#        "PASSWORD":"xxxxx",
#        "VERIFY":true,           <- Check TLS-Certificate (recommended)
#        "SHOW_SERVICES_UP":true, <- show service details for services in state UP
#        "SHOW_HOSTS_UP":true,    <- show host details for services in state UP
#        "OUTPUTFILE": false,     <- debug, insert "path/filename" instead false
#         "ADDFILTER": ""         <- filter for icinga API request, see below
#    },
#    {
#       ... repeat for an other icinga server, different to the first
#    }
#  ],
#" GLOBAL": {
#    "SHOW_OK_DETAILS": true    <- global switch, enable or disable details
#  }
#}

# Filter example:
#"ADDFILTER": "&&\"notify-group1\" in host.groups"
#"ADDFILTER": "&&\"ZFS Hosts\" in host.groups"

# If VERIFY is False you may have to disable SSL/TLS warnings too. (Not recommended.)
# Choose your poison.
#requests.packages.urllib3.disable_warnings(InsecureRequestWarning)

# Modify the line below if necessary
# configfile 
config_file = '.config/icinga2statuscfg.json'

import requests, json, urllib.parse
#from requests.packages.urllib3.exceptions import InsecureRequestWarning
#requests.packages.urllib3.disable_warnings(InsecureRequestWarning)
from pathlib import Path

# DEBUG
#import logging
#logging.basicConfig(level=logging.DEBUG)

TEMPLATE = '''
---
{icingahost}
🔄 Services OK:\t\t{n_svc_ok} | color=#44bb77 href={base_url}{svc_uri}0{svc_ok_det}
🔄 Services Crit:\t\t{n_svc_cr} | color=#cc2211 href={base_url}{svc_uri}2{svc_cr_det}
🔄 Services Warn:\t{n_svc_wn} | color=#ffaa44 href={base_url}{svc_uri}1{svc_wn_det}
🔄 Services Unkn:\t{n_svc_uk} | color=#aa44ff href={base_url}{svc_uri}3{svc_uk_det}
🔄 Services Ack:\t\t{n_svc_ac} | color=#f7cd76 href={base_url}{svc_ack_uri}1{svc_ack_det}
🔄 Services DT:\t\t{n_svc_dt} | color=#f7ed86 href={base_url}{svc_downtime_uri}1{svc_dt_det}
💻 Hosts Up:\t\t\t{n_hst_up} | color=#44bb77 href={base_url}{hst_uri}2{hst_up_det}
💻 Hosts Down:\t\t{n_hst_dw} | color=#cc2211 href={base_url}{hst_uri}1{hst_dw_det}
💻 Hosts Ack:\t\t{n_hst_ac} | color=#f7cd76 href={base_url}{hst_ack_uri}1{hst_ack_det}
💻 Hosts DT:\t\t\t{n_hst_dt} | color=#f7ed86 href={base_url}{hst_downtime_uri}1{hst_dt_det}'''

ERR_TPL = '''ERROR | color=#ff5566
---
{errors} | trim=true'''


s_stat = ''
s_col = ''

def setBaseURL(SERVER, PORT, server):
    API = server['APIURL'].format(server=SERVER, port=PORT)
    BASE_URL = server['BASEURL'].format(server=SERVER,port=PORT)
    TPL_DATA = {
        'icingahost': server['SERVERDISPLAYNAME'],
        'base_url': BASE_URL,
        'svc_uri': '/list/services?service_state=',
        'svc_ack_uri': '/list/services?service_acknowledged=',
        'svc_downtime_uri': '/list/services?service_in_downtime=',
        'hst_uri': '/list/hosts?host_state=',
        'hst_ack_uri': '/list/hosts?host_acknowledged=',
        'hst_downtime_uri': '/list/hosts?host_in_downtime='
    }
    return API, BASE_URL, TPL_DATA


def icinga_server(server):
    global s_stat, s_col
    TIMEOUT = 3
    USER = server['USER']
    PASSWORD = server['PASSWORD']
    # VERIFY = 'icingaca.crt'
    VERIFY = server['VERIFY']
    SHOW_SERVICES_UP = server['SHOW_SERVICES_UP']
    SHOW_HOSTS_UP = server['SHOW_HOSTS_UP']
    outputfile = server['OUTPUTFILE']
    ADDFILTER = server['ADDFILTER']

    output = ''
    stat=0
    num_hosts_down_notfiltered=0
    num_services_critical_notack=0
    num_services_warning_notack=0
    num_services_unknown_notack=0

    headers = {
        'Accept': 'application/json',
        'X-HTTP-Method-Override': 'GET'
    }
    res = '/v1/status/CIB'

    # default to SERVER1:PORT1
    (API, BASE_URL, TPL_DATA) = setBaseURL(server['SERVER1'], server['PORT1'], server)
    url = API + res
    #print(url)

    # open a Session
    sess = requests.Session()
    # First host
    try:
        resp = sess.post(url,
                            headers=headers,
                            auth=(USER, PASSWORD),
                            timeout=TIMEOUT,
                            verify=VERIFY)
    except requests.exceptions.Timeout:
        # ignore and try second host
        pass
    except requests.exceptions.TooManyRedirects:
        # print("Too many redirects")
        #return "Error: Too many redirects"
        s_col = '#44bb77'
        s_stat = "Error: Too many redirects " + s_stat 
        return ""
    except requests.exceptions.RequestException as e:
        #print(e)
        #return "Error: connection"
        s_col = '#44bb77'
        s_stat = "Error: connection " + s_stat
        return ""

    if (not 'resp' in vars() or resp.status_code != 200) and server['SERVER2']:
        # Second host
        try:
            #print(API)
            (API, BASE_URL, TPL_DATA) = setBaseURL(server['SERVER2'], server['PORT2'], server)
            url = API + res
            resp = sess.post(url,
                                headers=headers,
                                auth=(USER, PASSWORD),
                                timeout=TIMEOUT,
                                verify=VERIFY)
        except requests.exceptions.Timeout:
            #print("No connection")
            #return "Error: No connection"
            s_col = '#44bb77'
            s_stat = "Error: No connection " + s_stat
            return ""
        except requests.exceptions.TooManyRedirects:
            # print("Too many redirects")
            #return "Error: Too many redirects"
            s_col = '#44bb77'
            s_stat = "Error: Too many redirects " + s_stat 
            return ""
        except requests.exceptions.RequestException as e:
            #print(e)
            #return "Error: connection"
            s_col = '#44bb77'
            s_stat = "Error: connection " + s_stat
            return ""

    if resp.status_code != 200:
        print(ERR_TPL.format(errors=resp.text))
        return "Error"

    #print(url)
    # parse /v1/status/CIB data
    data = resp.json()
    status = data['results'][0]['status']
    num_services_ok = int(status['num_services_ok'])
    num_services_critical = int(status['num_services_critical'])
    num_services_warning = int(status['num_services_warning'])
    num_services_unknown = int(status['num_services_unknown'])
    num_services_acknowledged = int(status['num_services_acknowledged'])
    num_services_in_downtime = int(status['num_services_in_downtime'])
    num_hosts_up = int(status['num_hosts_up'])
    num_hosts_down = int(status['num_hosts_down'])
    num_hosts_acknowledged = int(status['num_hosts_acknowledged'])
    num_hosts_in_downtime = int(status['num_hosts_in_downtime'])

    #
    TPL_DATA['svc_ok_det'] = ''
    TPL_DATA['svc_cr_det'] = ''
    TPL_DATA['svc_wn_det'] = ''
    TPL_DATA['svc_uk_det'] = ''
    TPL_DATA['svc_ack_det'] = ''
    TPL_DATA['svc_dt_det'] = ''
    TPL_DATA['hst_up_det'] = ''
    TPL_DATA['hst_dw_det'] = ''
    TPL_DATA['hst_ack_det'] = ''
    TPL_DATA['hst_dt_det'] = ''

    if outputfile:
        output += "---\n"

    # Down hosts without Ack and Downtime
    if num_hosts_down > 0:
        #print("Hosts Down: \t"+str(TPL_DATA['n_hst_dw'])+"| color=red href="+BASE_URL+"/list/hosts?host_problem=1&sort=host_severity&host_unhandled=1")
        res = '/v1/objects/hosts'
        # filter hosts pending or in downtime or acknowleged
        data = {
            "attrs": [ "__name", "name", "state", "downtime_depth", "acknowledgement", "last_check_result" ],
            "filter": "host.state==1.0&&host.acknowledgement==0.0&&host.downtime_depth==0.0&&host.last_check_result!=null" + ADDFILTER
        }
        url = API + res
        try:
            resp = sess.post(url,
                                json = data,
                                headers=headers,
                                auth=(USER, PASSWORD),
                                timeout=TIMEOUT,
                                verify=VERIFY)
        except requests.exceptions.Timeout:
            # ignore and try Second host
            pass
        except requests.exceptions.TooManyRedirects:
            # print("Too many redirects")
            # return "Error: Too many redirects"
            s_col = '#44bb77'
            s_stat = "Error: Too many redirects " + s_stat
            return ""
        except requests.exceptions.RequestException as e:
            # print(e)
            # return "Error: connection"
            s_col = '#44bb77'
            s_stat = "Error: connection " + s_stat
            return ""
        if resp.status_code == 200:
            data = resp.json()
            num_hosts_down = len(data['results'])
            data['results'].sort(key=extract_host1, reverse=False)
            num_hosts_down_notfiltered = len(data['results'])
            if num_hosts_down_notfiltered > 0:
               stat = 3
            if outputfile:
                output += "Hosts Down: \t"+str(num_hosts_down_notfiltered)+"| color=red href="+BASE_URL+"/list/hosts?host_problem=1&sort=host_severity&host_unhandled=1\n"
            #thefile = open('/tmp/results.txt', 'w')
            for i in data['results']:
                #print(i, file=thefile)
                if outputfile:
                    output += "-- "+i['name']+"| href="+BASE_URL+"/host/show?host="+urllib.parse.quote(i['name']) + "\n"
                TPL_DATA['hst_dw_det'] += "\n-- "+i['name']+"| href="+BASE_URL+"/host/show?host="+urllib.parse.quote(i['name'])
                if i['attrs']['last_check_result'] != None:
                    if outputfile:
                        output += "---- "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/host/show?host="+urllib.parse.quote(i['name']) + "\n"
                    TPL_DATA['hst_dw_det'] += "\n---- "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/host/show?host="+urllib.parse.quote(i['name'])

        else:
            print(ERR_TPL.format(errors=resp.text))
            return ""

    # Hosts acknowledged
    if num_hosts_acknowledged > 0:
        if outputfile:
            output += "Hosts Ack: \t"+str(num_hosts_acknowledged)+"| color=#f7cd76 href="+BASE_URL+"/list/hosts?host_problem=1&sort=host_severity&host_unhandled=0" + "\n"
        res = '/v1/objects/hosts'
        data = {
            "attrs": [ "__name", "name", "state", "downtime_depth", "acknowledgement", "last_check_result" ],
            "filter": "host.acknowledgement!=0" + ADDFILTER
        }
        url = API + res
        try:
            resp = sess.post(url,
                                json = data,
                                headers=headers,
                                auth=(USER, PASSWORD),
                                timeout=TIMEOUT,
                                verify=VERIFY)
        except requests.exceptions.Timeout:
            # ignore and try Second host
            pass
        except requests.exceptions.TooManyRedirects:
            # print("Too many redirects")
            # return "Error: Too many redirects"
            s_col = '#44bb77'
            s_stat = "Error: Too many redirects " + s_stat
            return ""
        except requests.exceptions.RequestException as e:
            # print(e)
            # return "Error: connection"
            s_col = '#44bb77'
            s_stat = "Error: connection " + s_stat
            return ""
        if resp.status_code == 200:
            data = resp.json()
            num_hosts_acknowledged = len(data['results'])
            data['results'].sort(key=extract_host1, reverse=False)
            #thefile = open('/tmp/results.txt', 'w')
            for i in data['results']:
                #print(i, file=thefile)
                if outputfile:
                    output += "-- "+i['name']+"| href="+BASE_URL+"/host/show?host="+urllib.parse.quote(i['name']) + "\n"
                TPL_DATA['hst_ack_det'] += "\n-- "+i['name']+"| href="+BASE_URL+"/host/show?host="+urllib.parse.quote(i['name'])
                if i['attrs']['last_check_result'] != None:
                    if outputfile:
                        output += "---- "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/host/show?host="+urllib.parse.quote(i['name']) + "\n"
                    TPL_DATA['hst_ack_det'] += "\n---- "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/host/show?host="+urllib.parse.quote(i['name'])
        else:
            print(ERR_TPL.format(errors=resp.text))
            return ""

    # Hosts in Downtime
    if num_hosts_in_downtime > 0:
        if outputfile:
            output += "Hosts DT: \t"+str(num_hosts_in_downtime)+"| color=#f7ed86 href="+BASE_URL+"/list/hosts?host_in_downtime=1" + "\n"
        res = '/v1/objects/hosts'
        data = {
            "attrs": [ "__name", "name", "state", "downtime_depth", "acknowledgement", "last_check_result" ],
            "filter": "host.downtime_depth!=0" + ADDFILTER
        }
        url = API + res
        try:
            resp = sess.post(url,
                                json = data,
                                headers=headers,
                                auth=(USER, PASSWORD),
                                timeout=TIMEOUT,
                                verify=VERIFY)
        except requests.exceptions.Timeout:
            # ignore and try Second host
            pass
        except requests.exceptions.TooManyRedirects:
            # print("Too many redirects")
            # return "Error: Too many redirects"
            s_col = '#44bb77'
            s_stat = "Error: Too many redirects " + s_stat
            return ""
        except requests.exceptions.RequestException as e:
            # print(e)
            # return "Error: connection"
            s_col = '#44bb77'
            s_stat = "Error: connection " + s_stat
            return ""
        if resp.status_code == 200:
            data = resp.json()
            num_hosts_in_downtime = len(data['results'])
            data['results'].sort(key=extract_host1, reverse=False)
            #thefile = open('/tmp/results.txt', 'w')
            for i in data['results']:
                #print(i, file=thefile)
                if outputfile:
                    output += "-- "+i['name']+"| href="+BASE_URL+"/host/show?host="+urllib.parse.quote(i['name']) + "\n"
                TPL_DATA['hst_dt_det'] += "\n-- "+i['name']+"| href="+BASE_URL+"/host/show?host="+urllib.parse.quote(i['name'])
                if i['attrs']['last_check_result'] != None:
                    if outputfile:
                        output += "---- "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/host/show?host="+urllib.parse.quote(i['name']) + "\n"
                    TPL_DATA['hst_dt_det'] += "\n---- "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/host/show?host="+urllib.parse.quote(i['name'])
        else:
            print(ERR_TPL.format(errors=resp.text))
            return ""


    # Service Problems state Critical without Service Ack or Downtime and Host Ack or Host Downtime or (new) Host.state is CRITICAL (state==2) or UNKNOWN (STATE==3)
    if num_services_critical > 0:
        #print("Services Crit: \t"+str(TPL_DATA['n_svc_cr'])+"| color=#ff5566 href="+BASE_URL+"/list/services?service_state=21&sort=service_severity&service_unhandled=1")
        res = '/v1/objects/services?joins=host.name&joins=host.groups'
        data = {
            "attrs": [ "__name", "name", "state", "downtime_depth", "acknowledgement", "last_check_result" ],
            #"filter": "service.state==2.0&&service.acknowledgement==0.0&&service.downtime_depth==0.0&&host.acknowledgement==0.0&&host.downtime_depth==0"
            #"filter": "service.state==2.0&&service.acknowledgement==0.0&&service.downtime_depth==0.0&&host.acknowledgement==0.0&&host.downtime_depth==0&&host.state>1.0"
            "filter": "service.state==2.0&&service.acknowledgement==0.0&&service.downtime_depth==0.0&&host.acknowledgement==0.0&&host.downtime_depth==0&&host.state==0.0" + ADDFILTER
        }
        url = API + res
        try:
            resp = sess.post(url,
                                json = data,
                                headers=headers,
                                auth=(USER, PASSWORD),
                                timeout=TIMEOUT,
                                verify=VERIFY)
        except requests.exceptions.Timeout:
            # ignore and try Second host
            pass
        except requests.exceptions.TooManyRedirects:
            # print("Too many redirects")
            # return "Error: Too many redirects"
            s_col = '#44bb77'
            s_stat = "Error: Too many redirects " + s_stat
            return ""
        except requests.exceptions.RequestException as e:
            # print(e)
            # return "Error: connection"
            s_col = '#44bb77'
            s_stat = "Error: connection " + s_stat
            return ""
        if resp.status_code == 200:
            data = resp.json()
            data['results'].sort(key=extract_host1, reverse=False)
            num_services_critical_notack = len(data['results'])
            if num_services_critical_notack > 0:
               stat = 3
            if outputfile:
                output += "Services Crit: \t"+str(num_services_critical_notack)+"| color=#ff5566 href="+BASE_URL+"/list/services?service_state=21&sort=service_severity&service_unhandled=1" + "\n"
            lastHost = ''
            for i in data['results']:
                if i['joins']['host']['name'] != lastHost:
                    if outputfile:
                        output += "-- " + i['joins']['host']['name'] +"\n"
                    TPL_DATA['svc_cr_det'] += "\n-- " + i['joins']['host']['name']
                    lastHost = i['joins']['host']['name']
                if outputfile:
                    output += "---- "+i['attrs']['name']+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name']) + "\n"
                TPL_DATA['svc_cr_det'] += "\n---- "+i['attrs']['name']+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name'])
                if i['attrs']['last_check_result'] != None:
                    if outputfile:
                        output += "------ "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name']) + "\n"
                    TPL_DATA['svc_cr_det'] += "\n------ "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name'])
        else:
            print(ERR_TPL.format(errors=resp.text))
            return ""

    # Service Problems state Warning without Service Ack or Downtime and Host Ack or Host Downtime
    if num_services_warning > 0:
        res = '/v1/objects/services?joins=host.name&joins=host.groups'
        data = {
            "attrs": [ "__name", "name", "state", "downtime_depth", "acknowledgement", "last_check_result" ],
            "filter": "service.state==1.0&&service.acknowledgement==0.0&&service.downtime_depth==0.0&&host.acknowledgement==0.0&&host.downtime_depth==0" + ADDFILTER
        }
        url = API + res
        try:
            resp = sess.post(url,
                                json = data,
                                headers=headers,
                                auth=(USER, PASSWORD),
                                timeout=TIMEOUT,
                                verify=VERIFY)
        except requests.exceptions.Timeout:
            # ignore and try Second host
            pass
        except requests.exceptions.TooManyRedirects:
            # print("Too many redirects")
            # return "Error: Too many redirects"
            s_col = '#44bb77'
            s_stat = "Error: Too many redirects " + s_stat
            return ""
        except requests.exceptions.RequestException as e:
            # print(e)
            # return "Error: connection"
            s_col = '#44bb77'
            s_stat = "Error: connection " + s_stat
            return ""
        if resp.status_code == 200:
            data = resp.json()
            data['results'].sort(key=extract_host1, reverse=False)
            #thefile = open('/tmp/results.txt', 'w')
            #json.dump(data, thefile)
            num_services_warning_notack = len(data['results'])
            if num_services_warning_notack > 0 and stat == 0:
               stat = 2
            if outputfile:
                output += "Services Warn: \t"+str(num_services_warning_notack)+"| color=#ffaa44 href="+BASE_URL+"/list/services?service_state=1&sort=service_severity&service_unhandled=1" + "\n"
            lastHost = ''
            for i in data['results']:
                if i['joins']['host']['name'] != lastHost:
                    if outputfile:
                        output += "-- " + i['joins']['host']['name'] + "\n"
                    TPL_DATA['svc_wn_det'] += "\n-- " + i['joins']['host']['name']
                    lastHost = i['joins']['host']['name']
                if outputfile:
                    output += "---- "+i['attrs']['name']+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name']) + "\n"
                TPL_DATA['svc_wn_det'] += "\n---- "+i['attrs']['name']+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name'])
                if i['attrs']['last_check_result'] != None:
                    if outputfile:
                        output += "------ "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name']) + "\n"
                    TPL_DATA['svc_wn_det'] += "\n------ "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name'])
        else:
            print(ERR_TPL.format(errors=resp.text))
            return ""

    # Service Problems state Unknown without Service Ack or Downtime and Host Ack or Host Downtime
    if num_services_unknown > 0:
        res = '/v1/objects/services?joins=host.name&joins=host.groups'
        data = {
            "attrs": [ "__name", "name", "state", "downtime_depth", "acknowledgement", "last_check_result" ],
            "filter": "service.state==3.0&&service.acknowledgement==0.0&&service.downtime_depth==0.0&&host.acknowledgement==0.0&&service.last_check_result!=null&&host.downtime_depth==0" + ADDFILTER
        }
        url = API + res
        try:
            resp = sess.post(url,
                                json = data,
                                headers=headers,
                                auth=(USER, PASSWORD),
                                timeout=TIMEOUT,
                                verify=VERIFY)
        except requests.exceptions.Timeout:
            # ignore and try Second host
            pass
        except requests.exceptions.TooManyRedirects:
            # print("Too many redirects")
            # return "Error: Too many redirects"
            s_col = '#44bb77'
            s_stat = "Error: Too many redirects " + s_stat
            return ""
        except requests.exceptions.RequestException as e:
            # print(e)
            # return "Error: connection"
            s_col = '#44bb77'
            s_stat = "Error: connection " + s_stat
            return ""
        if resp.status_code == 200:
            data = resp.json()
            data['results'].sort(key=extract_host1, reverse=False)
            num_services_unknown_notack = len(data['results'])
            if num_services_unknown_notack > 0 and stat == 0:
               stat = 1
            if outputfile:
                output += "Services Unkn: \t"+str(num_services_unknown_notack)+"| color=#aa44ff href="+BASE_URL+"/list/services?service_state=3&sort=service_severity&service_unhandled=1" + "\n"
            #thefile = open('/tmp/results.txt', 'w')
            for i in data['results']:
                #print(i, file=thefile)
                if outputfile:
                    output += "-- "+i['joins']['host']['name']+" "+i['attrs']['name']+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name']) + "\n"
                TPL_DATA['svc_uk_det'] += "\n-- "+i['joins']['host']['name']+" "+i['attrs']['name']+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name'])
                if i['attrs']['last_check_result'] != None:
                    if outputfile:
                        output += "---- "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name']) +"\n"
                    TPL_DATA['svc_uk_det'] += "\n---- "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name'])
        else:
            print(ERR_TPL.format(errors=resp.text))
            return ""


    # Service Problems Acknowledged
    if num_services_acknowledged > 0:
        if outputfile:
            output += "Services Ack: \t"+str(num_services_acknowledged)+"| color=#f7cd76 href="+BASE_URL+"/list/services?service_state=1&sort=service_severity&service_unhandled=0" + "\n"
        res = '/v1/objects/services?joins=host.name&joins=host.groups'
        data = {
            "attrs": [ "__name", "name", "state", "downtime_depth", "acknowledgement", "last_check_result" ],
            "filter": "service.acknowledgement!=0" + ADDFILTER
        }
        url = API + res
        try:
            resp = sess.post(url,
                                json = data,
                                headers=headers,
                                auth=(USER, PASSWORD),
                                timeout=TIMEOUT,
                                verify=VERIFY)
        except requests.exceptions.Timeout:
            # ignore and try Second host
            pass
        except requests.exceptions.TooManyRedirects:
            # print("Too many redirects")
            # return "Error: Too many redirects"
            s_col = '#44bb77'
            s_stat = "Error: Too many redirects " + s_stat
            return ""
        except requests.exceptions.RequestException as e:
            # print(e)
            # return "Error: connection"
            s_col = '#44bb77'
            s_stat = "Error: connection " + s_stat
            return ""
        if resp.status_code == 200:
            data = resp.json()
            num_services_acknowledged = len(data['results'])
            data['results'].sort(key=extract_host1, reverse=False)
            lastHost = ''
            for i in data['results']:
                if i['joins']['host']['name'] != lastHost:
                    if outputfile:
                        output += "-- " + i['joins']['host']['name'] + "\n"
                    lastHost = i['joins']['host']['name']
                    TPL_DATA['svc_ack_det'] += "\n-- " + i['joins']['host']['name']
                if outputfile:
                    output += "---- "+i['attrs']['name']+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name']) + "\n"
                TPL_DATA['svc_ack_det'] += "\n---- "+i['attrs']['name']+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name'])
                if i['attrs']['last_check_result'] != None:
                    if outputfile:
                        output += "------ "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name']) + "\n"
                    TPL_DATA['svc_ack_det'] += "\n------ "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name'])
        else:
            print(ERR_TPL.format(errors=resp.text))
            return ""

    # Service in Downtime
    if num_services_in_downtime > 0:
        if outputfile:
            output += "Services Downtime: \t"+str(num_services_in_downtime)+"| color=#f7ed86 href="+BASE_URL+"/list/services?service_in_downtime=1" + "\n"
        res = '/v1/objects/services?joins=host.name&joins=host.groups'
        data = {
            "attrs": [ "__name", "name", "state", "downtime_depth", "acknowledgement", "last_check_result" ],
            "filter": "service.downtime_depth!=0" + ADDFILTER
        }
        url = API + res
        try:
            resp = sess.post(url,
                                json = data,
                                headers=headers,
                                auth=(USER, PASSWORD),
                                timeout=TIMEOUT,
                                verify=VERIFY)
        except requests.exceptions.Timeout:
            # ignore and try Second host
            pass
        except requests.exceptions.TooManyRedirects:
            # print("Too many redirects")
            # return "Error: Too many redirects"
            s_col = '#44bb77'
            s_stat = "Error: Too many redirects " + s_stat
            return ""
        except requests.exceptions.RequestException as e:
            # print(e)
            # return "Error: connection"
            s_col = '#44bb77'
            s_stat = "Error: connection " + s_stat
            return ""
        if resp.status_code == 200:
            data = resp.json()
            num_services_in_downtime = len(data['results'])
            data['results'].sort(key=extract_host1, reverse=False)
            lastHost = ''
            for i in data['results']:
                if i['joins']['host']['name'] != lastHost:
                    if outputfile:
                        output += "-- " + i['joins']['host']['name'] + "\n"
                    TPL_DATA['svc_dt_det'] += "\n-- " + i['joins']['host']['name']
                    lastHost = i['joins']['host']['name']
                if outputfile:
                    output += "---- "+i['attrs']['name']+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name']) + "\n"
                TPL_DATA['svc_dt_det'] += "\n---- "+i['attrs']['name']+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name'])
                if i['attrs']['last_check_result'] != None:
                    if outputfile:
                        output += "------ "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name']) + "\n"
                    TPL_DATA['svc_dt_det'] += "\n------ "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name'])
        else:
            print(ERR_TPL.format(errors=resp.text))
            return ""


    # TODO services and hosts that are OK, not needed to show, maybe
    if outputfile:
        output += "---\n"

    # UP Hosts 
    if num_hosts_up > 0:
        if outputfile:
            output += "Hosts Up: \t"+str(num_hosts_up)+"| color=green href="+BASE_URL+"/list/hosts?host_state=0" + "\n"
        res = '/v1/objects/hosts'
        data = {
            "attrs": [ "__name", "name", "state", "downtime_depth", "acknowledgement", "last_check_result" ],
            "filter": "host.state==0.0" + ADDFILTER
        }
        url = API + res
        try:
            resp = sess.post(url,
                             json=data,
                             headers=headers,
                             auth=(USER, PASSWORD),
                             timeout=TIMEOUT,
                             verify=VERIFY)
        except requests.exceptions.Timeout:
            # ignore and try Second host
            pass
        except requests.exceptions.TooManyRedirects:
            # print("Too many redirects")
            # return "Error: Too many redirects"
            s_col = '#44bb77'
            s_stat = "Error: Too many redirects " + s_stat
            return ""
        except requests.exceptions.RequestException as e:
            # print(e)
            # return "Error: connection"
            s_col = '#44bb77'
            s_stat = "Error: connection " + s_stat
            return ""
        if resp.status_code == 200:
            data = resp.json()
            num_hosts_up = len(data['results'])
            #thefile = open('/tmp/results.txt', 'w')
            #json.dump(sorted_data, thefile)
            if SHOW_HOSTS_UP:
                data['results'].sort(key=extract_host1, reverse=False)
                #sorted_data = dict(data)
                #sorted_data['results'] = sorted(data['results'], key=lambda k: k['attrs'].get('__name', 0), reverse=False)
                for i in data['results']:
                    #print(i, file=thefile)
                    if outputfile:
                        output += "-- "+i['name']+"| href="+BASE_URL+"/host/show?host="+urllib.parse.quote(i['name']) + "\n"
                        output += "---- "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/host/show?host="+urllib.parse.quote(i['name']) + "\n"
                    TPL_DATA['hst_up_det'] += "\n-- "+i['name']+"| href="+BASE_URL+"/host/show?host="+urllib.parse.quote(i['name'])
                    TPL_DATA['hst_up_det'] += "\n---- "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/host/show?host="+urllib.parse.quote(i['name'])
        else:
            print(ERR_TPL.format(errors=resp.text))
            return ""

    # UP services
    if num_services_ok > 0:
        if outputfile:
            output += "Services OK: \t"+str(num_services_ok)+"| color=green href="+BASE_URL+"/list/services?service_problem=0" + "\n"
        res = '/v1/objects/services?joins=host.name&joins=host.groups'
        data = {
            "attrs": [ "__name", "name", "state", "downtime_depth", "acknowledgement", "last_check_result" ],
            "filter": "service.state==0.0" + ADDFILTER
        }
        url = API + res
        try:
            resp = sess.post(url,
                                json = data,
                                headers=headers,
                                auth=(USER, PASSWORD),
                                timeout=TIMEOUT,
                                verify=VERIFY)
        except requests.exceptions.Timeout:
            # ignore and try Second host
            pass
        except requests.exceptions.TooManyRedirects:
            # print("Too many redirects")
            # return "Error: Too many redirects"
            s_col = '#44bb77'
            s_stat = "Error: Too many redirects " + s_stat
            return ""
        except requests.exceptions.RequestException as e:
            # print(e)
            # return "Error: connection"
            s_col = '#44bb77'
            s_stat = "Error: connection " + s_stat
            return ""
        if resp.status_code == 200:
            data = resp.json()
            #thefile = open('/tmp/results.txt', 'w')
            #json.dump(data, thefile)
            num_services_ok = len(data['results'])
            if SHOW_SERVICES_UP:
                data['results'].sort(key=extract_host1, reverse=False)
                lastHost=''
                for i in data['results']:
                    if i['joins']['host']['name'] != lastHost:
                        if outputfile:
                            output += "-- " + i['joins']['host']['name'] + "\n"
                        TPL_DATA['svc_ok_det'] += "\n-- " + i['joins']['host']['name']
                        lastHost = i['joins']['host']['name']
                    if outputfile:
                        output += "---- "+i['attrs']['name']+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name']) + "\n"
                        output += "------ "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name']) + "\n"
                    TPL_DATA['svc_ok_det'] += "\n---- "+i['attrs']['name']+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name'])
                    TPL_DATA['svc_ok_det'] += "\n------ "+i['attrs']['last_check_result']['output'].replace('\n', ' ').replace('\r', '').replace('|', ' ')+"| href="+BASE_URL+"/service/show?host="+urllib.parse.quote(i['joins']['host']['name'])+"&service="+urllib.parse.quote(i['attrs']['name'])
        else:
            print(ERR_TPL.format(errors=resp.text))
            return ""

    # closing the connection
    resp.close()

    # output data
    TPL_DATA['n_svc_ok'] = num_services_ok
    TPL_DATA['n_svc_cr'] = num_services_critical_notack
    TPL_DATA['n_svc_wn'] = num_services_warning_notack
    TPL_DATA['n_svc_uk'] = num_services_unknown_notack
    TPL_DATA['n_svc_ac'] = num_services_acknowledged
    TPL_DATA['n_svc_dt'] = num_services_in_downtime
    TPL_DATA['n_hst_up'] = num_hosts_up
    TPL_DATA['n_hst_dw'] = num_hosts_down_notfiltered
    TPL_DATA['n_hst_ac'] = num_hosts_acknowledged
    TPL_DATA['n_hst_dt'] = num_hosts_in_downtime


    if stat == 3:
        s_col = '#ff5566'
        s_stat += '🆘 ' + str(num_hosts_down_notfiltered + num_services_critical_notack) + ' '
    #if stat == 2:
    if num_services_warning_notack > 0:
        if s_col == '':
            s_col = '#ffaa44'
        s_stat += '⚠️ ' + str(num_services_warning_notack) + ' '
    #if stat == 1:
    if num_services_unknown_notack > 0:
        if s_col == '':
            s_col = '#aa44ff'
        s_stat += 'U' + str(num_services_unknown_notack)
    if stat != 3 and num_services_unknown_notack == 0 and num_services_unknown_notack == 0 :
        if s_col == '':
            s_col = '#44bb77'
        s_stat += '✅ ' + str(num_services_ok + num_hosts_up) + ' '

    if outputfile:
        thefile = open(outputfile, 'w')
        print(output, file=thefile)
    #print(TEMPLATE.format(**TPL_DATA))
    return TEMPLATE.format(**TPL_DATA)

def extract_host(json):
    try:
        return json['joins']['host']['name']
    except KeyError:
        return 0


def extract_host1(json):
    try:
        return json['attrs']['__name']
    except KeyError:
        return 0


def main():
    global s_stat, s_col
    # Json Config File
    # to set below homedir use the expansion
    with open(Path().home() / config_file) as json_data_file:
        cfgdata = json.load(json_data_file)
    # or use relative path:
    #with open(Path().absolute() / config_file) as json_data_file:
    #    cfgdata = json.load(json_data_file)
    # print(cfgdata)

    output=''
    for server in cfgdata["SERVER"]:
        output += icinga_server(server)
    if s_stat:
        output = "{} | color={}".format(s_stat,s_col) + output
    print(output)


if __name__ == '__main__':
    main()
