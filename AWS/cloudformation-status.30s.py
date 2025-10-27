#!/usr/bin/env /Users/sergio.pena/.asdf/installs/python/3.13.2/bin/python

# -*- coding: utf-8 -*-

# <xbar.title>Cloudformation Status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Sergio Pena</xbar.author>
# <xbar.author.github>sergiopena</xbar.author.github>
# <xbar.desc>Monitor the status of your Cloudformation stacks</xbar.desc>
# <xbar.dependencies>python,boto3,pydantic</xbar.dependencies>
# <xbar.image>https://raw.githubusercontent.com/sergiopena/xbar-cloudformation-status/main/imgs/example.png</xbar.image>
# <xbar.abouturl>https://github.com/sergiopena/xbar-cloudformation-status/blob/main/README.md</xbar.abouturl>

# <xbar.var>string(VAR_STACKS_REGEX="Pipeline-Name"): Regex to filter the stacks to monitor. Multiple stacks can be separated with pipes.</xbar.var>
# <xbar.var>string(VAR_STACKS_TITLE="CLOUDFORMATION STATUS"): Title to display in the plugin.</xbar.var>
# <xbar.var>string(VAR_STACKS_PROFILE="dev"): Profile to use for the AWS CLI.</xbar.var>

import boto3
from botocore.exceptions import NoCredentialsError, NoRegionError, UnauthorizedSSOTokenError
from datetime import datetime
from pydantic.dataclasses import dataclass
from typing import Optional
from pydantic.tools import parse_obj_as
import re
from os import getenv


RUNNING_STATES = ['CREATE_IN_PROGRESS', 'UPDATE_IN_PROGRESS', 'DELETE_IN_PROGRESS', 'ROLLBACK_IN_PROGRESS', 'UPDATE_ROLLBACK_IN_PROGRESS']

EMOTICONS_MAP = {
        'CREATE_COMPLETE': ':white_check_mark:',
        'UPDATE_COMPLETE': ':white_check_mark:',
        'CREATE_IN_PROGRESS': ':hourglass_flowing_sand:',
        'UPDATE_IN_PROGRESS': ':hourglass_flowing_sand:',
        'DELETE_IN_PROGRESS': ':hourglass_flowing_sand:',
        'CREATE_FAILED': ':x:',
        'UPDATE_FAILED': ':x:',
        'DELETE_FAILED': ':x:',
        'ROLLBACK_IN_PROGRESS': ':arrows_counterclockwise:',
        'ROLLBACK_COMPLETE': ':warning:',
        'ROLLBACK_FAILED': ':x:',
        'UPDATE_ROLLBACK_IN_PROGRESS': ':arrows_counterclockwise:',
        'UPDATE_ROLLBACK_COMPLETE': ':warning:',
        'UPDATE_ROLLBACK_FAILED': ':x:',
        'REVIEW_IN_PROGRESS': ':hourglass_flowing_sand:',
        'IMPORT_IN_PROGRESS': ':hourglass_flowing_sand:',
        'IMPORT_COMPLETE': ':white_check_mark:',
        'IMPORT_ROLLBACK_IN_PROGRESS': ':arrows_counterclockwise:',
        'IMPORT_ROLLBACK_COMPLETE': ':warning:',
        'IMPORT_ROLLBACK_FAILED': ':x:'
    }

@dataclass
class Stack:
    StackName: str
    StackStatus: str
    StackId: str
    LastUpdatedTime: Optional[datetime] = None
    CreationTime: Optional[datetime] = None

@dataclass
class StackList:
    StackSummaries: list[Stack]

class CloudformationStatus:
    def __init__(self):
        self.session = boto3.Session(profile_name=getenv('VAR_STACKS_PROFILE', 'dev'))
        self.client = self._get_client()
        self.stacks = self._list_stacks()
        self.running: bool = False

    def _get_client(self):
        try:
            return self.session.client('cloudformation')
        except NoRegionError:
            print(':alert: No credentials found')
            exit(0)
        except NoCredentialsError:
            print(':alert: No region found')
            exit(0)

    def get_stacks(self):
        try:
            paginator = self.client.get_paginator('list_stacks')
            for page in paginator.paginate():
                items = parse_obj_as(StackList, page)
        except UnauthorizedSSOTokenError:
            print(f':alert: Expired SSO token')
            exit(0)
        return items

    def _list_stacks(self):
        stacks = []
        try:
            paginator = self.client.get_paginator('list_stacks')
            for page in paginator.paginate():
                items = parse_obj_as(StackList, page)
                for item in items.StackSummaries:
                    stacks.append(item)
        except UnauthorizedSSOTokenError:
            print(f':alert: Expired SSO token')
            exit(0)
        return stacks


    def get_stacks(self, regex: str): 
        stacks_by_name = {}
        for stack in self.stacks:
            if re.search(regex, stack.StackName):
                # Skip stacks with DELETE_COMPLETE status
                if stack.StackStatus == 'DELETE_COMPLETE':
                    continue
                # Skip stacks that contain 'RotationSingleUser'
                if 'RotationSingleUser' in stack.StackName:
                    continue
                                
                display_time = stack.LastUpdatedTime if stack.LastUpdatedTime is not None else stack.CreationTime
                if stack.StackStatus in RUNNING_STATES:
                    self.running = True
                
                if stack.StackName not in stacks_by_name or display_time > stacks_by_name[stack.StackName]['time']:
                    stacks_by_name[stack.StackName] = {
                        'status': stack.StackStatus,
                        'time': display_time
                    }

        return stacks_by_name
    
    def print_stacks(self, stacks_by_name: dict):
        # Get the AWS region from the session
        region = self.session.region_name or 'eu-west-1'

        output = []
        for stack_name in sorted(stacks_by_name.keys()):
            stack_info = stacks_by_name[stack_name]
            status = stack_info['status']
            emoticon = EMOTICONS_MAP.get(status, ':question:')
            
            # Create the CloudFormation stack URL
            stack_url = f"https://{region}.console.aws.amazon.com/cloudformation/home?region={region}#/stacks/stackinfo?stackId={stack_name}"
            
            output.append(f"{emoticon} {stack_name}")
            output.append(f"--Last updated: {stack_info['time']}")
            output.append(f"--Status: {stack_info['status']}")
            output.append(f"--Open in AWS Console | href={stack_url}")
        return output


if __name__ == "__main__":
    cloudformation_status = CloudformationStatus()
    title = getenv('VAR_STACKS_TITLE', 'CLOUDFORMATION STATUS')
    output = []

    for regex in getenv('VAR_STACKS_REGEX').split('|'):
        output.append('---')
        output.append(f"Stack: {regex}")
        stacks_by_name = cloudformation_status.get_stacks(regex)
        output.extend(cloudformation_status.print_stacks(stacks_by_name))
    

    print(f'{title} {":hourglass_flowing_sand:" if cloudformation_status.running else ""} ')
    print(f'{":hourglass_flowing_sand:" if cloudformation_status.running else ""} {title }')
    for line in output:
        print(line)
    print('---')
    print('Refresh | refresh=true')
