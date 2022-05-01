#! /usr/bin/python3

# <xbar.title>Display network latency</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.author>Ciaran Finnegan</xbar.author>
# <xbar.author.github>slartibastfast</xbar.author.github>
# <xbar.desc>Display current network round trip time (latency) for ICMP to a defined target</xbar.desc>
# <swiftbar.hideRunInTerminal>true</swiftbar.hideRunInTerminal>

import subprocess
import re
newline = '\n'

target = 'syd1.speedtest.telstra.net' # replace with your target fully qualified domain name or IP address
targetName ='Sydney' # replace with the target name you wish to appear in the Menu bar


result = subprocess.run(['/sbin/ping','-c','5','-W','1000',target], capture_output=True, text=True).stdout

resultMatch = re.search('packets received, ([0-9\.]+)% packet loss\nround-trip min\/avg\/max\/stddev = ([0-9\.]+)\/([0-9\.]+)\/([0-9\.]+)\/([0-9\.]+)', result)
loss = str(round(float(resultMatch.group(1))))
rttMin = str(round(float(resultMatch.group(2))))
rttAvg = str(round(float(resultMatch.group(3))))
rttMax = str(round(float(resultMatch.group(4))))
jitter = str(round(float(resultMatch.group(5))))


if int(rttAvg) <= 300 : color = ''
if int(rttAvg) in range(300,500,1) : color = 'orange'
if int(rttAvg) >500 : color = 'red'


theOutput = (f':bolt.fill: {targetName} {rttAvg} ms | color={color} sfcolor={color}'
             f'---{newline}'
             f'Ping target: {target}'
             f'{newline}Packet Loss {loss}%'
             f'{newline}Round Trip Min {rttMin}ms'
             f'{newline}Round Trip Avg {rttAvg}ms'
             f'{newline}Round Trip Max {rttMax}ms'
             f'{newline}Jitter {jitter}ms')


print (theOutput)



