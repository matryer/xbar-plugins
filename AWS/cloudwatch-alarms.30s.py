#!/usr/local/bin/python3
# -*- coding: UTF-8 -*-

# <bitbar.title>CloudWatch Alarms Status</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Sebastian Rodriguez</bitbar.author>
# <bitbar.author.github>sebasrp</bitbar.author.github>
# <bitbar.desc>Monitor the status of your CloudWatch Alarms</bitbar.desc>
# <bitbar.dependencies>python, boto3</bitbar.dependencies>
# <bitbar.image>https://i.imgur.com/qiqHX32.png</bitbar.image>

import boto3


class CWA_status:

    def __init__(self):
        self.cw_client = boto3.client('cloudwatch')
        self.cwa_OK = self.get_alarms(state='OK')
        self.cwa_NOK = self.get_alarms(state='ALARM')
        self.cwa_ID = self.get_alarms(state='INSUFFICIENT_DATA')

    def get_alarms(self, state):
        alarms = []
        paginator = self.cw_client.get_paginator('describe_alarms')
        for response in paginator.paginate(StateValue=state):
            alarms += response['MetricAlarms']
        alarms = sorted(alarms, key=lambda i: i['StateUpdatedTimestamp'])
        return alarms

    def display_alarms(self):
        status_string = 'CWA '
        cwa_nok = f"{':fire: ' + str(len(self.cwa_NOK)) if self.cwa_NOK else ''}"
        cwa_id = f"{':warning: ' + str(len(self.cwa_ID)) if self.cwa_ID else ''}"
        cwa_ok = f"{':thumbsup:' if not self.cwa_NOK and not self.cwa_ID else ''}"
        print(f"{status_string}{cwa_nok}{' - ' if (cwa_nok and cwa_id) else ''}{cwa_id}{cwa_ok}")

    def display_detailed_alarms(self):
        self.display_alarm_list(self.cwa_NOK, 'ALARM')
        self.display_alarm_list(self.cwa_ID, 'INSUFFICIENT_DATA')
        self.display_alarm_list(self.cwa_OK, 'OK')

    def display_alarm_list(self, alarm_dict, state_string):
        print(f"{state_string} - ({len(alarm_dict)})")
        if alarm_dict:
            for monitor in alarm_dict:
                print(f"-- {monitor['AlarmName']}")
        else:
            print(f"-- No monitors in {state_string}!")


if __name__ == '__main__':
    try:
        cwa_status = CWA_status()
        cwa_status.display_alarms()
        print('---')
        cwa_status.display_detailed_alarms()
    except Exception as ex:
        print(f":warning: Exception executing script. Exception: {ex}")
        raise ex
