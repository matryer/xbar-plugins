#!/usr/bin/env python3

# <xbar.title>Homebridge Controller</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Eric Andrechek</xbar.author>
# <xbar.author.github>EricAndrechek</xbar.author.github>
# <xbar.desc>Control Homebridge and connected devices.</xbar.desc>
# <xbar.image>https://user-images.githubusercontent.com/35144594/102293908-0b849200-3f16-11eb-9778-7ce25edcc7ec.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://homebridge.io/</xbar.abouturl>

from urllib.request import Request, urlopen
from urllib.error import URLError, HTTPError
import json
import pathlib
import os
import sys

homebridgeIcon = "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"
offlineIcon = "iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAhGVYSWZNTQAqAAAACAAFARIAAwAAAAEAAQAAARoABQAAAAEAAABKARsABQAAAAEAAABSASgAAwAAAAEAAgAAh2kABAAAAAEAAABaAAAAAAAAAJAAAAABAAAAkAAAAAEAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAJKADAAQAAAABAAAAJAAAAAA4NgJpAAAACXBIWXMAABYlAAAWJQFJUiTwAAACaGlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNS40LjAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyIKICAgICAgICAgICAgeG1sbnM6ZXhpZj0iaHR0cDovL25zLmFkb2JlLmNvbS9leGlmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICAgICA8dGlmZjpSZXNvbHV0aW9uVW5pdD4yPC90aWZmOlJlc29sdXRpb25Vbml0PgogICAgICAgICA8ZXhpZjpDb2xvclNwYWNlPjE8L2V4aWY6Q29sb3JTcGFjZT4KICAgICAgICAgPGV4aWY6UGl4ZWxYRGltZW5zaW9uPjY3NTwvZXhpZjpQaXhlbFhEaW1lbnNpb24+CiAgICAgICAgIDxleGlmOlBpeGVsWURpbWVuc2lvbj42NzU8L2V4aWY6UGl4ZWxZRGltZW5zaW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KHeXkngAABWFJREFUWAmlmFuIVmUUhvcctROVU5l2mKKBsqFMJiGoZDp5lTkX6kVBERNDlFdlQRfRXAhCUJE3QVTQiQSj8kYwuqtAK4kysNQwKCqICoqItLLn3bPePevf/56Z/zfhZa1vHd+9vu/b+3eKovZvY7GxT6iZG5cniqIljnUP6G0MPhnjeDHe3wWZAfWAwCJwC1jpnuj9Se9jPQgGQlY+xzTKsbGpgeliuqOno/BiFUEuA5+AE4FtLs5aBBrrYZ+flCZzEmTOp/BBIDK7wXehP2tSkth6wWpwXeBM23NcpXd5ZgajWCbzWthW0PAHIILPh+009PfD5ikeZX11+FvOoGzFeDE9//jKqPJJfWbOo6An82q4S4H9cvAtUPOnwdbQDyDfACa3Rwms2wnlgnPpJHoy56J/BdSwnEwU1sH2uRpGN6m/InZdxC1nLdvX7oXeeM7sb5MkLIpimozJvN4WmAzEXQAOARH/F2yIGpeh/w0OORy9c0IE+6mXon8J1KAkg+yJJqvQXw5ckxrpAQ4D5UxE7Aj6P7KnuM4IkeTJNB3gsggx48DbosbHwA2p2RBrTWoyEdKEuiNEgs+MJtNygFl7MmvRRULYDB4M/Tjy+kRK27cqCF2MrgkdSf75J0Rwvk3epvptuo04k9mSim8Ju6ZQkUp+kROhg0FQn5ryAR3TInF6Mo23KYq0kSFPt8xbbFJqXJJCnh65umV6kO+Be5V5LUQi2AXnvE0UWRcFy22qF8Hn6T4ScSJVnimkL8i74XvL+fZ5rReUC+UzU79N66OQyDzgZOWCleBC2ZDlixbpSWn78kFfwvpToDq7gM/kzEsSgwtof3UjFFieGaSDJ8Iu3/2JjHLeC9+fyKkg5Qc0qeP4MqmzWJvUh+g3R97secK4G6jhS24YQRvCLt899qEvA/okyP4Z0PZIfyjyyidm/XDY66R0Tg+H7w/kEtfWmM8GvwM5yqeLoneyVhPhbiegK95P+E7E3hRxin00bH5fmVR9+yYjZw9yZstSE0/olSi2KTXIZDTuj8O3y/mRswb7sfA9ETZvu7dPhNeETz9N9DnxherJh3AYxxGghC9CSr/LTdEbyWDPH9cbWes8KXdrjdRjYZd/xHVzTGnD6XfCcvQPIuln5CYnoTeSsT9LYq8FvwGRelI+pM/UM2F/POyn5txKJ8ik+tHHwFI70XVVPwJqUN+m1dheANvBpSlHNfRQytkuO/IcsDds5TFAb34xRkJ1oFNh/erz1OpkbsWn26Omgt7AK1Lulax/CZ9+EewLPb8UF/yW9ZGkM1GOEnlvFClvU2qWP676sD4VcT8ir0hx+mn7TfhEemfyzU/GgZIk+oU5FcV22M/6jrCpweZkN6mfsF+V7Dp/94H1ydZ6ze2YS5JsQjo/B4CavwgmQtd6UvnIaqvR9VtaPt2kilTug73zydQS/Qm4hCL+rKhZJqNLoJ8R5QMEwW0RszfWet8M5pjcpytdhaKoPhW6sjvA7WHTeaueFv2MsF+ErsP+K1gcttnvlAz/5x9FS1K5BjaRqZqg+22rny/7gab4nHKQ1fRyjQa9txjnf60NjjaTioJyGkj95PAnQdvlKQ6ht3zjVMixbUVrhtHR0cFirBgb6OIPDGqetyhPSJP5HGgyb7oXeke3STwK/21hZjE7fhdbSNLMU9Kr4SgQmbedh97R9LVLbTs1WjCu9PQuOpd0M+QwEBFhn+PR286dfVmKSAwkm0u9pxtSNPR0dLZ2An0i1qoS8pS26g2G+chU4SLVNr7K26rQ2Idc75kheZEdnZlu+pQ3L6ZVHdpWKrMrkwoyC8brAo0UI3pFVJdjtlpR/AchkkzScviaWgAAAABJRU5ErkJggg=="

def printer(content):
    if type(content) is list:
        for line in content:
            print(line)
    elif content != "---" and content != "--":
        print(content)
    else:
        print(content)


def doStuff(token, url, icon, command, unit):
    headers = {
        'accept': '*/*',
        'Authorization': 'Bearer {}'.format(token),
    }

    if command:
        if command == "restart":
            req = Request('{}/api/server/restart'.format(url), headers=headers, method='PUT')
        elif command == "reboot":
            req = Request('{}/api/platform-tools/linux/restart-host'.format(url), headers=headers, method='PUT')
        urlopen(req)
    else:
        cpu = ""
        temp = ""
        ram = ""
        updates = []
        numUpdates = 0
        uptime = ""
        status = ""
        state = False

        req = Request('{}/api/plugins'.format(url), headers=headers)
        updatesRequest = urlopen(req)

        if updatesRequest.code == 200:
            for plugin in json.loads(updatesRequest.read()):
                name = plugin['name']
                update = plugin['updateAvailable']
                link = "https://github.com/" + plugin['author'] + '/' + plugin['name'] + '/releases/latest'
                try:
                    name = plugin['displayName']
                except:
                    pass
                if update is True:
                    numUpdates += 1
                updates.append("{} v{} - {} | href={}".format(name, plugin['installedVersion'], "up to date" if not update else "new update v{}".format(plugin['latestVersion']), link))
        req = Request('{}/api/status/nodejs'.format(url), headers=headers)
        nodeJSRequest = urlopen(req)
        if nodeJSRequest.code == 200:
            nodeVersion = json.loads(nodeJSRequest.read())
            updates.append("NodeJS {} - {} | href=https://github.com/nodejs/node/releases/latest".format(nodeVersion['currentVersion'], "up to date" if not nodeVersion['updateAvailable'] else "new update {}".format(nodeVersion['latestVersion'])))
            numUpdates += 1 if nodeVersion['updateAvailable'] else 0
        req = Request('{}/api/status/homebridge-version'.format(url), headers=headers)
        homebridgeRequest = urlopen(req)
        if homebridgeRequest.code == 200:
            hbVersion = json.loads(homebridgeRequest.read())
            updates.append("Homebridge v{} - {} | href=https://github.com/homebridge/homebridge/releases/latest".format(hbVersion['installedVersion'], "up to date" if not hbVersion['updateAvailable'] else "new update v{}".format(hbVersion['latestVersion'])))
            numUpdates += 1 if hbVersion['updateAvailable'] else 0
        numUpdates = "Avaliable Updates: " + str(numUpdates)

        req = Request('{}/api/status/cpu'.format(url), headers=headers)
        cpuRequest = urlopen(req)
        if cpuRequest.code == 200:
            cpuRequestJson = json.loads(cpuRequest.read())
            cpu = "CPU: " + str(round(float(cpuRequestJson["currentLoad"]))) + "%"
            try:
                init_temp = float(cpuRequestJson["cpuTemperature"]["main"])
                if unit == "F":
                    init_temp = str(round((init_temp * (9/5)) + 32))
                else:
                    init_temp = str(round(init_temp)) 
                temp = "Temperature: " + init_temp + "˚" + unit
            except:
                pass
        
        req = Request('{}/api/status/ram'.format(url), headers=headers)
        ramRequest = urlopen(req)
        if ramRequest.code == 200:
            ramRequestJson = json.loads(ramRequest.read())
            ram = "RAM: " + str(100 - round(int(ramRequestJson["mem"]["available"]) / int(ramRequestJson["mem"]["total"]) * 100)) + "%"

        req = Request('{}/api/status/uptime'.format(url), headers=headers)
        uptimeRequest = urlopen(req)
        if uptimeRequest.code == 200:
            uptime = "Uptime: " + str(round(round(float(json.loads(uptimeRequest.read())["processUptime"])) / 86400)) + " days"
        
        req = Request('{}/api/status/homebridge'.format(url), headers=headers)
        statusRequest = urlopen(req)
        if statusRequest.code == 200:
            if json.loads(statusRequest.read())["status"] == "up":
                state = True
            upWord = "up"
            downWord = "down"
            status = "Homebridge is " + upWord if state else downWord
        
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
            ico = "| templateImage={}".format(homebridgeIcon) if state else "| image={}".format(offlineIcon)
        print(ico)
        printer('---')
        printer('Open Homebridge UI | href={}'.format(url))
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
        print('Reboot Server | bash="' + str(__file__) + '" param1=reboot terminal=false')

def login(username, password, url, icon, command, unit, config):
    headers = {
        'accept': '*/*',
        'Content-Type': 'application/json',
    }
    data = {
        "username": username,
        "password": password
    }

    req = Request('{}/api/auth/login'.format(url), headers=headers, data=json.dumps(data).encode("utf-8"))
    response = urlopen(req)
    if response.code == 201:
        token = json.loads(response.read())['access_token']
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
            newConfig.write("unit = \"{}\"\n".format(unit))
            newConfig.write(lastSection)
        doStuff(token, url, icon, command, unit)
    else:
        printer('❌') # login failed

config = pathlib.Path.home().joinpath('.config', 'xbar_homebridge')
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
        file.write("unit = \"C\"\n")
    os.system("open " + str(config))
else:
    token = ""
    username = ""
    password = ""
    url = ""
    icon = ""
    unit = ""
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
        elif line.split(" = ")[0] == "unit":
            unit = line.split(" = ")[1].split("\"")[1]
    if token:
        headers = {
            'accept': '*/*',
            'Authorization': 'Bearer {}'.format(token),
        }

        req = Request('{}/api/auth/check'.format(url), headers=headers)
        try:
            checkToken = urlopen(req)
            if checkToken.code == 200:
                doStuff(token, url, icon, command, unit)
        except HTTPError as e:   
            if e.code == 401:
                login(username, password, url, icon, command, unit, config)
    else:
        login(username, password, url, icon, command, unit, config)
