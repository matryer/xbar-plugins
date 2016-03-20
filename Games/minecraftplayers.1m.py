#!/usr/bin/python
# -*- coding: utf-8 -*-

# Bitbar plugin showing number of connected players on a Minecraft server

# <bitbar.title>Minecraft connected players</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Páll Hilmarsson</bitbar.author>
# <bitbar.author.github>pallih</bitbar.author.github>
# <bitbar.desc>Displays number of connected players on a Minecraft server</bitbar.desc>
# <bitbar.image>http://i.imgur.com/ImBlVHD.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>

# Heavily based on https://github.com/Fanobii/minestatus-checker

import socket
import struct
import json

# Define your server ip address and port

HOST = "IP ADDRESS"
PORT = 25565


def unpack_varint(s):
    d = 0
    for i in range(5):
        b = ord(s.recv(1))
        d |= (b & 0x7F) << 7*i
        if not b & 0x80:
            break
    return d


def pack_varint(d):
    o = ""
    while True:
        b = d & 0x7F
        d >>= 7
        o += struct.pack("B", b | (0x80 if d > 0 else 0))
        if d == 0:
            break
    return o


def pack_data(d):
    return pack_varint(len(d)) + d


def pack_port(i):
    return struct.pack('>H', i)


def get_info(host='localhost', port=25565):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((host, port))
    s.send(pack_data("\x00\x00" + pack_data(host.encode('utf8')) + pack_port(port) + "\x01"))
    s.send(pack_data("\x00"))
    unpack_varint(s)
    unpack_varint(s)
    l = unpack_varint(s)
    d = ""
    while len(d) < l:
        d += s.recv(1024)
    s.close()
    return d.decode('utf8')


try:
    result = json.loads(get_info(host=HOST, port=PORT))
    if result['players']['online'] > 0:
        print "☑️ " + str(result['players']['online'])
        print "---"
        for player in result['players']['sample']:
            print player['name']
    else:
        print "🔘 " + str(result['players']['online'])
except socket.error:
    print "✖️"
    print "---"
    print "Connection error"

print "---"
print "Start Minecraft | bash=/usr/bin/open param1=-a param2=Minecraft terminal=false"
