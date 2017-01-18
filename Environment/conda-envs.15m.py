#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Anaconda Environments</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Darius Morawiec</bitbar.author>
# <bitbar.author.github>nok</bitbar.author.github>
# <bitbar.desc>Useful BitBar plugin to list all created conda environments and to open a new session with a chosen environment.</bitbar.desc>
# <bitbar.image>https://github.com/nok/conda-envs/blob/master/themes/dark.png?raw=true</bitbar.image>
# <bitbar.dependencies>conda</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/nok/conda-envs</bitbar.abouturl>


import os
import subprocess


CONDA_PATH = '~/anaconda/bin/conda'
CHECK_VERSION = True


class Color:
    GREEN = '#3bb15c'
    BLUE = '#4a90f3'


class Env:
    def __init__(self, env_name):
        conda = os.path.expanduser(CONDA_PATH)
        cmd = [conda, 'env', 'export', '-n', env_name]
        deps = subprocess.check_output(cmd, stderr=subprocess.STDOUT).strip()

        version = None
        if CHECK_VERSION:
            for dep in deps.splitlines():
                if '- python=' in dep:
                    version = dep.split('=')[1]
                    env_name += ' (%s)' % version
                    break

        self.env_name = env_name
        self.version = version

    @property
    def color(self):
        if self.version is None:
            return Color.WHITE
        else:
            major = self.version.split('.')[0]
            if major.startswith('2'):
                return Color.GREEN
        return Color.BLUE

    def __str__(self):
        if self.version is None:
            return ('%s | bash=source param1=activate '
                    'param2=%s terminal=true refresh=false') % (
                   self.env_name, self.env_name)
        return ('%s | color=%s bash=source param1=activate '
                'param2=%s terminal=true refresh=false') % (
               self.env_name, self.color, self.env_name)


def is_conda_installed():
    conda = os.path.expanduser(CONDA_PATH)
    try:
        subprocess.check_output([conda], stderr=subprocess.STDOUT).strip()
    except:
        print('---')
        print('Download Aanaconda | href=https://www.continuum.io/downloads')
        exit(-1)


def get_conda_envs():
    conda = os.path.expanduser(CONDA_PATH)
    cmd = [conda, 'env', 'list']
    out = subprocess.check_output(cmd, stderr=subprocess.STDOUT).strip()
    envs = []
    for env in out.splitlines():
        if not env.strip().startswith('#'):
            tuple = env.split()
            name = tuple[0]
            # path = tuple[1]
            envs.append(Env(name))
    return envs


def print_menu(envs):
    if len(envs) > 0:
        print('---')
        for idx, env in enumerate(envs):
            print(env)
        if CHECK_VERSION:
            print('---')
            print('Python 2 | color=%s' % Color.GREEN)
            print('Python 3 | color=%s' % Color.BLUE)
    print('---')
    conda = os.path.expanduser(CONDA_PATH)
    cmd = [conda, '--version']
    ver = subprocess.check_output(cmd, stderr=subprocess.STDOUT).strip()
    print(ver)


def main():
    print('𝗔')
    is_conda_installed()
    envs = get_conda_envs()
    print_menu(envs)


if __name__ == "__main__":
    main()
