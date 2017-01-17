#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Anaconda Environments</bitbar.title>
# <bitbar.version>v1.2</bitbar.version>
# <bitbar.author>Darius Morawiec</bitbar.author>
# <bitbar.author.github>nok</bitbar.author.github>
# <bitbar.desc>Useful BitBar plugin to list all created conda environments and to open a new session with a chosen environment.</bitbar.desc>
# <bitbar.image>https://github.com/nok/conda-envs/blob/master/themes/dark.png?raw=true</bitbar.image>
# <bitbar.dependencies>conda</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/nok/conda-envs</bitbar.abouturl>


import os
import subprocess as subp


# User settings:
CONDA_PATH = '~/anaconda/bin/conda'
CHECK_VERSION = True
CHECK_SIZE = True

# BitBar related constants:
LINE = '---'  # cutting line


class Color:
    GREEN = '#3bb15c'
    BLUE = '#4a90f3'


class Env:
    def __init__(self, name, path):
        conda = os.path.expanduser(CONDA_PATH)
        cmd = [conda, 'env', 'export', '-n', name]
        deps = subp.check_output(cmd, stderr=subp.STDOUT).strip()

        self.name = name

        if CHECK_SIZE:
            du = ['du', '-hs', path]
            size = subp.check_output(du, stderr=subp.STDOUT).strip().split()[0]
            name += ' (%s)' % size

        version = None
        if CHECK_VERSION:
            for dep in deps.splitlines():
                if '- python=' in dep:
                    version = dep.split('=')[1]
                    name += ' (%s)' % version
                    break

        self.meta = name
        self.version = version

    @property
    def color(self):
        """
        Return the color to the used Python version.

        Python 2.X: #3bb15c
        Python 3.X: #4a90f3

        :return: string: The color in hexadecimal format.
        """
        return Color.GREEN if self.version.startswith('2') else Color.BLUE

    def __str__(self):
        """
        Return the environment settings in BitBar format.

        :return: string: The environment settings in BitBar format.
        """
        cmd = '{meta} | bash=source param1=activate param2={name} ' + \
              'terminal=true refresh=false'
        meta = self.__dict__
        if self.version is not None:
            cmd += ' color={color}'
            meta.update({'color': self.color})
        return cmd.format(**meta)


def is_conda_installed():
    """
    Check whether conda is installed locally.

    :return: bool: Check whether conda is installed locally.
    """
    conda = os.path.expanduser(CONDA_PATH)
    try:
        subp.check_output([conda], stderr=subp.STDOUT).strip()
    except:
        print(LINE)
        print('Download Aanaconda | href=https://www.continuum.io/downloads')
        exit(-1)


def get_conda_envs():
    """
    Create a list of all parsed environments.

    :return: list: The list of environment instances.
    """
    conda = os.path.expanduser(CONDA_PATH)
    cmd = [conda, 'env', 'list']
    out = subp.check_output(cmd, stderr=subp.STDOUT).strip()
    envs = []
    for env in out.splitlines():
        if not env.strip().startswith('#'):
            tuple = env.split()
            name = tuple[0]
            path = tuple[1]
            try:
                env = Env(name, path)
                envs.append(env)
            except:
                pass
    return envs


def print_menu(envs):
    """
    Print the BitBar menu.

    :param envs: The parsed environment instances.
    """
    if len(envs) > 0:
        print(LINE)
        for idx, env in enumerate(envs):
            print(env)
        if CHECK_VERSION:
            print(LINE)
            print('Python 2 | color=%s' % Color.GREEN)
            print('Python 3 | color=%s' % Color.BLUE)
    print(LINE)
    conda = os.path.expanduser(CONDA_PATH)
    cmd = [conda, '--version']
    ver = subp.check_output(cmd, stderr=subp.STDOUT).strip()
    print(ver)


def main():
    print('𝗔')  # Print always the letter 'A' of 'Anaconda'
    is_conda_installed()
    envs = get_conda_envs()
    print_menu(envs)


if __name__ == "__main__":
    main()
