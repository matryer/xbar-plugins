#!/usr/bin/env PYTHONIOENCODING=UTF-8 python3

# <bitbar.title>Homebridge Controller</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Eric Andrechek</bitbar.author>
# <bitbar.author.github>EricAndrechek</bitbar.author.github>
# <bitbar.desc>Control Homebridge and connected devices.</bitbar.desc>
# <bitbar.image>https://user-images.githubusercontent.com/35144594/102293908-0b849200-3f16-11eb-9778-7ce25edcc7ec.png</bitbar.image>
# <bitbar.dependencies>python, requests module</bitbar.dependencies>
# <bitbar.abouturl>https://homebridge.io/</bitbar.abouturl>

import requests
import json
import pathlib
import os
import sys


def printer(content):
    if type(content) is list:
        for line in content:
            print(line + " | color=white")
    elif content != "---" and content != "--":
        print(content + " | color=white")
    else:
        print(content)


def doStuff(token, url, icon, command):
    headers = {
        'accept': '*/*',
        'Authorization': 'Bearer {}'.format(token),
    }

    if command:
        if command == "restart":
            requests.put('{}/api/server/restart'.format(url), headers=headers)
    else:
        cpu = ""
        temp = ""
        ram = ""
        updates = []
        numUpdates = 0
        uptime = ""
        status = ""
        state = False

        updatesRequest = requests.get('{}/api/plugins'.format(url), headers=headers)
        if updatesRequest.status_code == 200:
            for plugin in updatesRequest.json():
                name = plugin['name']
                update = plugin['updateAvailable']
                link = ""
                try:
                    name = plugin['displayName']
                except:
                    pass
                try:
                    link = plugin['links']["homepage"]
                except:
                    pass
                if update is True:
                    numUpdates += 1
                updates.append("{} - {} | href={}".format(name, "up to date" if not update else "new update", link))
        numUpdates = "Avaliable Updates: " + str(numUpdates)

        cpuRequest = requests.get('{}/api/status/cpu'.format(url), headers=headers)
        if cpuRequest.status_code == 200:
            cpu = "CPU: " + str(round(float(cpuRequest.json()["currentLoad"]))) + "%"
            try:
                temp = "Temperature: " + str(round(float(cpuRequest.json()["cpuTemperature"]["main"]))) + "˚C"
            except:
                pass
        
        ramRequest = requests.get('{}/api/status/ram'.format(url), headers=headers)
        if ramRequest.status_code == 200:
            ram = "RAM: " + str(round((int(ramRequest.json()["mem"]["used"]) / int(ramRequest.json()["mem"]["total"])) * 100)) + "%"
        
        uptimeRequest = requests.get('{}/api/status/uptime'.format(url), headers=headers)
        if uptimeRequest.status_code == 200:
            uptime = "Uptime: " + str(round(round(float(uptimeRequest.json()["processUptime"])) / 86400)) + " days"
        
        statusRequest = requests.get('{}/api/status/homebridge'.format(url), headers=headers)
        if statusRequest.status_code == 200:
            status = "Homebridge is " + statusRequest.json()["status"] + ""
            if statusRequest.json()["status"] == "up":
                state = True
        
        ico = ""
        if icon.upper() == "CPU":
            ico = cpu.split(": ")[1]
        elif icon.upper() == "RAM":
            ico = ram.split(": ")[1]
        elif icon.upper() == "TEMP":
            ico = temp.split(": ")[1]
        elif icon.upper() == "UPTIME":
            ico = uptime.split()[1] + "d"
        elif icon.upper() == "UPDATES":
            ico = numUpdates.split(": ")[1]
        else:
            ico = "✅" if state else "❌"
        print(ico)
        printer('---')
        printer(status)
        printer(numUpdates)
        printer(uptime)
        printer('---')
        printer(cpu)
        printer(ram)
        printer(temp)
        printer('---')
        printer(updates)
        printer('---')
        print('Restart Homebridge | bash="' + str(__file__) + '" param1=restart terminal=false')

def login(username, password, url, icon, command, config):
    headers = {
        'accept': '*/*',
        'Content-Type': 'application/json',
    }
    data = {
        "username": username,
        "password": password
    }
    response = requests.post('{}/api/auth/login'.format(url), headers=headers, data=json.dumps(data))
    if response.status_code == 201:
        token = response.json()['access_token']
        firstSection = ""
        lastSection = ""
        with open(config, 'r') as oldConfig:
            conf = oldConfig.read().split('[homebridge]')
            firstSection = conf[0]
            try:
                lastSection = "[" + conf[1].split("[")[1]
            except IndexError:
                pass
        with open(config, 'w') as newConfig:
            newConfig.write(firstSection)
            newConfig.write('[homebridge]\n')
            newConfig.write("# edit the username and password to your homebridge's and then save and close\n")
            newConfig.write("# you can also modify the url from the default if desired\n")
            newConfig.write("# to change the icon image, specify whether you want to see: \"CPU\", \"TEMP\", \"RAM\", \"UPTIME\", \"UPDATES\", or \"STATUS\". Defaults to \"STATUS\"\n")
            newConfig.write("username = \"{}\"\n".format(username))
            newConfig.write("password = \"{}\"\n".format(password))
            newConfig.write("url = \"{}\"\n".format(url))
            newConfig.write("icon = \"{}\"\n".format(icon))
            newConfig.write("token = \"{}\"\n".format(token))
            newConfig.write(lastSection)
        doStuff(token, url, icon, command)
    else:
        printer('❌') # login failed

config = pathlib.Path.home().joinpath('.config', 'bitbar', 'config')
config.touch(exist_ok=True)

command = None
if len(sys.argv) > 1:
    command = sys.argv[1]

hbConfig = []
with open(config, 'r') as file:
    for section in file.read().split('['):
        if section.split('\n')[0] == 'homebridge]':
            hbConfig = section.split('\n')[3:]
if hbConfig == []:
    with open(config, "a") as file:
        file.write("\n[homebridge]\n")
        file.write("# edit the username and password to your homebridge's and then save and close\n")
        file.write("# you can also modify the url from the default if desired\n")
        file.write("# to change the icon image, specify whether you want to see: \"CPU\", \"TEMP\", \"RAM\", \"UPTIME\", \"UPDATES\", or \"STATUS\". Defaults to \"STATUS\"\n")
        file.write("username = \"\"\n")
        file.write("password = \"\"\n")
        file.write("url = \"http://homebridge.local\"\n")
        file.write("icon = \"STATUS\"\n")
        file.write("token = \"\"\n")
    os.system("open " + str(config))
else:
    token = ""
    username = ""
    password = ""
    url = ""
    icon = ""
    for line in hbConfig:
        if line.split(" = ")[0] == "username":
            username = line.split(" = ")[1].split("\"")[1]
        elif line.split(" = ")[0] == "password":
            password = line.split(" = ")[1].split("\"")[1]
        elif line.split(" = ")[0] == "token":
            token = line.split(" = ")[1].split("\"")[1]
        elif line.split(" = ")[0] == "url":
            url = line.split(" = ")[1].split("\"")[1]
        elif line.split(" = ")[0] == "icon":
            icon = line.split(" = ")[1].split("\"")[1]
    if token:
        headers = {
            'accept': '*/*',
            'Authorization': 'Bearer {}'.format(token),
        }

        checkToken = requests.get('{}/api/auth/check'.format(url), headers=headers)
        if checkToken.status_code == 401:
            login(username, password, url, icon, command, config)
        elif checkToken.status_code == 200:
            doStuff(token, url, icon, command)
        else:
            printer("❌")
    else:
        login(username, password, url, icon, command, config)
