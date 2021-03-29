#!/usr/local/bin/python3

# <xbar.title>UptimeRobot Monitor</xbar.title>
# <xbar.version>v0.1</xbar.version>
# <xbar.author>Finn LeSueur</xbar.author>
# <xbar.author.github>finnito</xbar.author.github>
# <xbar.desc>Get UptimeRobot statistics for an account.</xbar.desc>
# <xbar.image>https://uptimerobot.com/assets/img/logo_plain.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://gitlab.com/Finnito/bitbar-uptime_robot</xbar.abouturl>

import http.client
import json

# Insert your Read-Only API Key.
API_KEY = "ur381063-288bfb3885e923a80db0f3c5"

def main():
    """ The main call for the script."""
    response = getMonitors()
    numMonitors = len(response["monitors"])
    upMonitors = countUpMonitors(response)
    outputArray = parseMonitors(response)
    output = makeOutput(
        numMonitors,
        upMonitors,
        outputArray
    )
    print(output)


def getMonitors():
    """ Sets up and makes the request
    to UptimeRobot to get all monitors
    for an account.
    """
    conn = http.client.HTTPSConnection("api.uptimerobot.com")

    payload = "api_key={0}&format=json&logs=0&response_times_average=30&response_times=1".format(API_KEY)

    headers = {
        'content-type': "application/x-www-form-urlencoded",
        'cache-control': "no-cache"
        }
     
    conn.request("POST", "/v2/getMonitors", payload, headers)
     
    res = conn.getresponse()
    data = res.read()
    resp = data.decode("utf-8")
    resp = json.loads(resp)
    return resp


def countUpMonitors(resp):
    """ Iterates over the response
    from getMonitors() to count the number
    of monitors with response code 2.
    """
    upMonitors = 0
    for monitor in resp["monitors"]:
        if monitor["status"] == 2:
            upMonitors += 1
    return upMonitors


def parseMonitors(resp):
    """ Iterates over the repsonse
    from getMonitors to create a string
    with the name of the monitor and
    its status using emoji.
    """
    output = []
    fmtString = "{1}  {0} ({3}ms)|href={2}"

    for monitor in resp["monitors"]:
        status = monitor["status"]
        name = monitor["friendly_name"]
        url = monitor["url"]
        responseTime = int(float(monitor["average_response_time"]))
        if (status == 0):
            output.append(fmtString.format(name, "⏸", url, responseTime))
        elif (status == 1):
            output.append(fmtString.format(name, "❔", url, responseTime))
        elif (status == 2):
            output.append(fmtString.format(name, "🟢", url, responseTime))
        elif (status == 8):
            output.append(fmtString.format(name, "🟠", url, responseTime))
        elif (status == 9):
            output.append(fmtString.format(name, "🔴", url, responseTime))
        else:
            output.append(fmtString.format(name, "⁉️", url, responseTime))
    return output


def makeOutput(numMonitors, upMonitors, outputArray):
    """ Does string formatting to
    output the result in the format
    required by BitBar.
    """
    outputString = """{0}/{1} 🆙
---
{2}
Icon Definitions
--⏸: Paused
--❔: Not Yet Checked
--🟢: Up
--🟠: Seems Down
--🔴: Down
--⁉️: Unknown
""".format(
        upMonitors,
        numMonitors,
        "\n".join(outputArray)
    )
    return outputString


if __name__== "__main__":
  main()
