#!/usr/bin/env python
# -*- coding: utf-8 -*-
# <bitbar.title>Meta Package Manager</bitbar.title>
# <bitbar.version>v2.1.1</bitbar.version>
# <bitbar.author>Kevin Deldycke</bitbar.author>
# <bitbar.author.github>kdeldycke</bitbar.author.github>
# <bitbar.desc>List outdated packages and manage upgrades.</bitbar.desc>
# <bitbar.dependencies>python,mpm</bitbar.dependencies>
# <bitbar.image>https://i.imgur.com/CiQpQ42.png</bitbar.image>
# <bitbar.abouturl>https://github.com/kdeldycke/meta-package-manager</bitbar.abouturl>

"""
Default update cycle is set to 7 hours so we have a chance to get user's
attention once a day. Higher frequency might ruin the system as all checks are
quite resource intensive, and Homebrew might hit GitHub's API calls quota.
"""

from __future__ import print_function, unicode_literals

import json
import os
import sys
from operator import itemgetter
from subprocess import PIPE, Popen

PY2 = sys.version_info[0] == 2


def fix_environment():
    """ Tweak environment variable to find non-default system-wide binaries.

    macOS does not put ``/usr/local/bin`` or ``/opt/local/bin`` in the ``PATH``
    for GUI apps. For some package managers this is a problem. Additioanlly
    Homebrew and Macports are using different pathes. So, to make sure we can
    always get to the necessary binaries, we overload the path. Current
    preference order would equate to Homebrew, Macports, then system.
    """
    os.environ['PATH'] = ':'.join([
        '/usr/local/bin',
        '/usr/local/sbin',
        '/opt/local/bin',
        '/opt/local/sbin',
        os.environ.get('PATH', '')])

    # Python 3 Surrogate Handling. See:
    # http://click.pocoo.org/6/python3/#python-3-surrogate-handling
    os.environ['LC_ALL'] = os.environ['LANG'] = 'en_US.UTF-8'


def run(*args):
    """ Run a shell command, return error code, output and error message. """
    assert isinstance(args, tuple)
    try:
        process = Popen(args, stdout=PIPE, stderr=PIPE)
    except OSError:
        return None, None, "`{}` executable not found.".format(args[0])
    output, error = process.communicate()
    return (
        process.returncode,
        output.decode('utf-8') if output else None,
        error.decode('utf-8') if error else None)


def echo(message):
    """ Print message to the output.

    Not unlike ``click.echo()``, this method is required to support
    discrepencies in the way strings are handled in different Python versions
    and platforms.
    """
    if PY2:
        message = message.encode('utf-8')
    print(message)


def print_error_header():
    """ Generic header for blockng error. """
    echo("❌ | dropdown=false")
    echo("---")


def print_error(message):
    """ Print a formatted error line by line, in red. """
    for line in message.strip().split("\n"):
        echo(
            "{} | color=red font=Menlo size=10 trim=false "
            "emojize=false".format(line))


def print_menu():
    """ Print menu structure using BitBar's plugin API.

    See: https://github.com/matryer/bitbar#plugin-api
    """
    # Search for generic mpm CLI on system.
    code, _, error = run('mpm')
    if code or error:
        print_error_header()
        print_error(error)
        echo("---")
        echo(
            "Install / upgrade `mpm` CLI. | bash=pip param1=install "
            # TODO: Add minimal requirement on Python package.
            "param2=--upgrade param3=meta-package-manager terminal=true "
            "refresh=true".format(error))
        return

    # Fetch list of all outdated packages from all package manager available on
    # the system.
    _, output, error = run(
        'mpm', '--output-format', 'json', 'outdated', '--cli-format', 'bitbar')

    if error:
        print_error_header()
        print_error(error)
        return

    # Sort outdated packages by manager's name.
    managers = sorted(json.loads(output).values(), key=itemgetter('name'))

    # Print menu bar icon with number of available upgrades.
    total_outdated = sum([len(m['packages']) for m in managers])
    total_errors = len([True for m in managers if m['error']])
    echo("↑{}{} | dropdown=false".format(
        total_outdated,
        " ⚠️{}".format(total_errors) if total_errors else ""))

    # Print a full detailed section for each manager.
    for manager in managers:
        echo("---")

        if manager['error']:
            print_error(manager['error'])

        echo("{} outdated {} package{} | emojize=false".format(
            len(manager['packages']),
            manager['name'],
            's' if len(manager['packages']) != 1 else ''))

        if manager.get('upgrade_all_cli'):
            echo("Upgrade all | {} terminal=false refresh=true".format(
                manager['upgrade_all_cli']))

        for pkg_info in manager['packages']:
            echo(
                "{name} {installed_version} → {latest_version} | {upgrade_cli}"
                " terminal=false refresh=true emojize=false".format(
                    **pkg_info))


if __name__ == '__main__':
    fix_environment()
    print_menu()
