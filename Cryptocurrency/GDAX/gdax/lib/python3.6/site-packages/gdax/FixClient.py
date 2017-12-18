#! /usr/bin/env python

#	Copyright 2012 Johan Astborg
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

import asyncore, socket
import os
import datetime

SOH = '\x01'

TAGS = {
    8: 'BeginString',
    9: 'BodyLength',
    35: 'MsgType',
    49: 'SenderCompID',
    56: 'TargetCompID',
    34: 'MsgSeqNum',
    52: 'SendingTime',
    10: 'CheckSum',
    98: 'EncryptMethod',
    108: 'HeartBtInt'
}

TAGSR = {
    'BeginString': 8,
    'BodyLength': 9,
    'MsgType': 35,
    'SenderCompID': 49,
    'TargetCompID': 56,
    'MsgSeqNum': 34,
    'SendingTime': 52,
    'CheckSum': 10,
    'EncryptMethod': 98,
    'HeartBtInt': 108
}

MSGTYPES = {
    'A': 'Logon',
    '0': 'HeartBeat',
    '1': 'Test Request',
    '2': 'Resend Request',
    '4': 'Sequence Reset',
    '5': 'Logout',
    '8': 'ExecutionReport'
}

MSGTYPESR = {
    'Logon': 'A',
    'HeartBeat': '0',
    'Test Request': '1',
    'Resend Request': '2',
    'Sequence Reset': '4',
    'Logout': '5',
    'ExecutionReport': '8'
}

HEADER = [
    'BeginString',
    'SenderCompID',
    'TargetCompID',
]

class FixClient(asyncore.dispatcher):
    def __init__(self, host, port, senderId, targetId):
        self.senderId = senderId
        self.targetId = targetId
        asyncore.dispatcher.__init__(self)
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        self.connect((host, port))
        self.buffer = self.logon_message() + '\x00'

    def handle_connect(self):
        self.send(self.buffer)
        pass

    def handle_close(self):
        self.close()

    def handle_read(self):
        print "----------INBOUND----------"
        msg = self.recv(8192)
        self.parse(msg)

    def writable(self):
        return (len(self.buffer) > 0)

    def handl_expt(self):
        self.handle_error()

        def handle_error(self):
            print "Error"

    def handle_write(self):
        print "----------OUTBOUND----------"
        self.parse(self.buffer)
        sent = self.send(self.buffer)
        self.buffer = self.buffer[sent:]
        pass

    def current_datetime(self):
        return datetime.datetime.utcnow().strftime("%Y%m%d-%H:%M:%S.%f")[:-3]

    def make_tag(self, tag, value):
        print "%s=%s" % (TAGSR[tag], value)

    def pack(self, msgs):
        # Create body
        body = []

        if 'SenderCompID' not in msgs:
            print 'ERROR'
            return
        else:
            body.append("%i=%s" % (TAGSR['SenderCompID'], msgs['SenderCompID']))

        if 'TargetCompID' not in msgs:
            print 'ERROR'
            return
        else:
            body.append("%i=%s" % (TAGSR['TargetCompID'], msgs['TargetCompID']))

        if 'MsgSeqNum' not in msgs:
            print 'ERROR'
            return
        else:
            body.append("%i=%s" % (TAGSR['MsgSeqNum'], msgs['MsgSeqNum']))

        if 'SendingTime' not in msgs:
            print 'ERROR'
            return
        else:
            body.append("%i=%s" % (TAGSR['SendingTime'], msgs['SendingTime']))

        if 'EncryptMethod' not in msgs:
            print 'ERROR'
            return
        else:
            body.append("%i=%s" % (TAGSR['EncryptMethod'], msgs['EncryptMethod']))

        if 'HeartBtInt' not in msgs:
            print 'ERROR'
            return
        else:
            body.append("%i=%s" % (TAGSR['HeartBtInt'], msgs['HeartBtInt']))

        # Enable easy change when debugging
        SEP = SOH

        body = SEP.join(body) + SEP

        # Create header
        header = []
        header.append("%s=%s" % (TAGSR['BeginString'], 'FIX.4.2'))
        header.append("%s=%s" % (TAGSR['SenderCompID'], self.senderId))
        header.append("%s=%s" % (TAGSR['TargetCompID'], self.targetId))
        #header.append("%s=%s" % (TAGSR['MsgType'], MSGTYPESR[msgs['MsgType']]))
        #header.append("%s=%i" % (TAGSR['BodyLength'], len(body)))

        fixmsg = SEP.join(header) + SEP + body
        cksum = sum([ord(i) for i in list(fixmsg)]) % 256
        fixmsg = fixmsg + "%i=0%s" % (TAGSR['CheckSum'], cksum)

        return fixmsg + SEP

    def parse(self, rawmsg):
        msg = rawmsg.rstrip(os.linesep).split(SOH)
        msg = msg[:-1]
        msgs = {}

        print "|".join(msg)

        for m in msg:
            tag, value = m.split('=', 1)
            if int(tag) not in TAGS:
                print "Not valid: %s" % (tag)
            else:
                t = TAGS[int(tag)]
                if t == 'CheckSum':
                    cksum = ((sum([ord(i) for i in list(SOH.join(msg[:-1]))]) + 1) % 256)
                    if cksum == int(value):
                        print "CheckSum\t%s (OK)" % (int(value))
                    else:
                        print "CheckSum\t%s (INVALID)" % (int(value))
                elif t == 'MsgType':
                    msgs[t] = MSGTYPES[value]
                    print "MsgType\t\t%s" % msgs[t]
                else:
                    msgs[t] = value

    def logon_message(self):
        msgs = {}
        msgs['SendingTime'] = self.current_datetime()
        msgs['SenderCompID'] = self.senderId
        msgs['TargetCompID'] = self.targetId
        msgs['MsgSeqNum'] = 12
        msgs['EncryptMethod'] = 0
        msgs['MsgType'] = 'Logon'
        msgs['HeartBtInt'] = 30
        return self.pack(msgs)