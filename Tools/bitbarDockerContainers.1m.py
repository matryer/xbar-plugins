#!/usr/bin/env PYTHONIOENCODING=UTF-8 python3
# -*- encoding: utf-8 -*-

# <bitbar.title>Manage docker containers</bitbar.title>
# <bitbar.author>Andre Litty</bitbar.author>
# <bitbar.author.github>DerAndre</bitbar.author.github>
# <bitbar.image>https://i.imgur.com/jFu2b4V.png</bitbar.image>
# <bitbar.desc>View docker containers state, start or stop containers</bitbar.desc>
# <bitbar.dependencies>Python3</bitbar.dependencies>
# <bitbar.version>v1.0</bitbar.version>

import os
import sys
import subprocess

# Path to local docker executable, differs from system to system
DOCKER_PATH = '/usr/local/bin/docker'
FILE_PATH = os.path.realpath(__file__)
WHALE = """
                  ##         .
            ## ## ##        ==
        ## ## ## ## ##    ===
    /'''''''''''''''''''\___/ ===
~~~ {~~ ~~~~ ~~~ ~~~~ ~~~ ~ /  ===- ~~~
    \______ o           __/
        \    \         __/
         \____\_______/
"""


def run_subprocess(command):
    return (subprocess.run(command, check=True, stdout=subprocess.PIPE))


def get_containers(filter_running=False):
    # Return list docker containers (dict: id, name, state)
    # if filter_running return only running containers, else return all containers
    containers = []
    args = [DOCKER_PATH, 'container', 'ps']
    if filter_running:
        args.extend(['--filter', 'status=running'])
    else:
        args.append('-a')
    args.extend(['--format', '{{.ID}} {{.Names}} {{.Status}}'])
    try:
        result = run_subprocess(args).stdout.decode()
        # If result is empty (no containers found)
        if result == '':
            return containers
        result = result.split('\n')
        # Remove blank line after string split if exists
        if '' in result:
            result.remove('')
        for element in result:
            containers.append({
                'id': element.split(' ')[0],
                'name': element.split(' ')[1],
                'state': element.split(' ')[2],
            })
        return containers
    except subprocess.CalledProcessError:
        return containers


def stop_containers(containers):
    for container in containers:
        try:
            run_subprocess([DOCKER_PATH, 'stop', container['id']])
        except subprocess.CalledProcessError:
            continue
    return True


def to_terminal():
    try:
        result = run_subprocess([DOCKER_PATH, 'ps', '-a']).stdout.decode()
        print(result)
    except subprocess.CalledProcessError as e:
        print('Error while calling docker: ', e)
    print(WHALE + '#######################################')
    print('###          ENJOY DOCKER!          ###')
    print('#######################################\n')


def main():
    # Check if script is called with args (first args is script path by default)
    if(len(sys.argv) >= 2):
        if sys.argv[1] == "--stop":
            containers = get_containers(filter_running=True)
            stop_containers(containers)
            sys.exit(1)
        elif sys.argv[1] == "--console":
            to_terminal()
            sys.exit(1)
    try:
        docker_version = run_subprocess(
            [DOCKER_PATH, '-v']).stdout.decode()
        docker_version = docker_version.replace('\n', '')
    except subprocess.CalledProcessError:
        docker_version = 'Could not get docker version...'

    print(u"\u2693")
    print("---")
    print(f'{docker_version} | size=16 color=white')
    print("---")

    containers = get_containers()
    running_containers = False

    # List all docker containers sorted by state (running first)
    # On click: start / stop container depending on current state
    for container in sorted(containers, key=lambda c: c['state'], reverse=True):
        if container["state"] == 'Up':
            color = 'green'
            action = 'stop'
            running_containers = True
        elif container["state"] == 'Exited':
            color = '#c80000'
            action = 'start'
        else:
            color = 'white'
            action = 'start'
        print(
            f'{container["id"]} - {container["name"]}: {container["state"]} | trim=true color={color}\
                bash={DOCKER_PATH} param1={action} param2={container["id"]} terminal=false refresh=true')

    print("---")
    if running_containers:
        print(u"\U0001F6D1" + " Stop all | trim=false, color=white bash=" + FILE_PATH +
              " param1=--stop terminal=false refresh=true")
    else:
        print('Nothing to stop')

    print("View in terminal | trim=false, color=white bash=" + FILE_PATH +
          " param1=--console terminal=true refresh=false")


if __name__ == '__main__':
    main()
