#!/usr/bin/env PYTHONIOENCODING=UTF-8 python3
# -*- encoding: utf-8 -*-

# <bitbar.title>Manage docker containers</bitbar.title>
# <bitbar.author>Andre Litty</bitbar.author>
# <bitbar.author.github>DerAndre</bitbar.author.github>
# <bitbar.image>https://imgur.com/a/EGgp04H</bitbar.image>
# <bitbar.desc>View docker containers state, start or stop containers</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.version>v1.0</bitbar.version>

import os
import sys
import argparse
import subprocess


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


def get_containers(take_all=True):
    containers = []
    args = [DOCKER_PATH, 'container', 'ps']
    if take_all:
        args.append('-a')
    else:
        args.extend(['--filter', 'status=running'])
    args.extend(['--format', '{{.ID}} {{.Names}} {{.Status}}'])
    try:
        result = run_subprocess(args).stdout.decode()
        if result == '':
            return containers
        result = result.split('\n')
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
    parser = argparse.ArgumentParser()
    parser.add_argument('--stop', action='store_true',
                        help='Stop all docker container')
    if(len(sys.argv) >= 2):
        if sys.argv[1] == "--stop":
            containers = get_containers(take_all=False)
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
