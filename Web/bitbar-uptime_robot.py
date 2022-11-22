#!/usr/local/bin/python3

# <xbar.title>UptimeRobot Monitor</xbar.title>
# <xbar.version>v0.1</xbar.version>
# <xbar.author>Finn LeSueur</xbar.author>
# <xbar.author.github>finnito</xbar.author.github>
# <xbar.desc>Get UptimeRobot statistics for an account.</xbar.desc>
# <xbar.image>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</xbar.image>
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
