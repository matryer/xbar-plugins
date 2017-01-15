#!/usr/bin/env python
# -*- coding: utf-8 -*-
# <bitbar.title>Meta Package Manager</bitbar.title>
# <bitbar.version>v2.3.0</bitbar.version>
# <bitbar.author>Kevin Deldycke</bitbar.author>
# <bitbar.author.github>kdeldycke</bitbar.author.github>
# <bitbar.desc>List outdated packages and manage upgrades.</bitbar.desc>
# <bitbar.dependencies>python,mpm</bitbar.dependencies>
# <bitbar.image>https://i.imgur.com/CiQpQ42.png</bitbar.image>
# <bitbar.abouturl>https://github.com/kdeldycke/meta-package-manager</bitbar.abouturl>

"""
Bitbar plugin for Meta Package Manager (a.k.a. the mpm CLI).

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


# Set to ``False`` to replace the default flat layout with an alternative
# structure where all upgrade actions are put into submenus, one for each
# manager.
FLAT_LAYOUT = True


# Make it easier to change font, sizes and colors of the output
# See https://github.com/matryer/bitbar#writing-plugins for details
# An alternate "good looking" font is "font=NotoMono size=13" (not installed
# on MacOS by default though) that matches the system font quite well.
FONTS = {
    'normal':  '',                              # Use default system font
    'summary': '',                              # Package summary
    'package': '',                              # Indiviual packages
    'error':   'color=red font=Menlo size=12',  # Errors
}
# Use a monospaced font when using submenus
if not FLAT_LAYOUT:
    FONTS['summary'] = 'font=Menlo size=12'


def fix_environment():
    """Tweak environment variable to find non-default system-wide binaries.

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
    # https://click.pocoo.org/6/python3/#python-3-surrogate-handling
    os.environ['LC_ALL'] = os.environ['LANG'] = 'en_US.UTF-8'


def run(*args):
    """Run a shell command, return error code, output and error message."""
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
    """Print message to the output.

    Not unlike ``click.echo()``, this method is required to support
    discrepencies in the way strings are handled in different Python versions
    and platforms.
    """
    if PY2:
        message = message.encode('utf-8')
    print(message)


def print_error_header():
    """Generic header for blockng error."""
    echo("❌ | dropdown=false")
    echo("---")


def print_error(message, submenu=""):
    """Print a formatted error line by line.

    A red, fixed-width font is used to preserve traceback and exception layout.
    """
    for line in message.strip().split("\n"):
        echo(
            "{}{} | {f_error} trim=false emojize=false"
            "".format(submenu, line, f_error=FONTS['error']))


def print_package_items(packages, submenu=""):
    """Print a menu entry for each outdated packages available for upgrade."""
    for pkg_info in packages:
        echo(
            "{}{name} {installed_version} → {latest_version} | {upgrade_cli}"
            " terminal=false refresh=true {f_package} emojize=false".format(
                submenu, f_package=FONTS['package'], **pkg_info))


def print_upgrade_all_item(manager, submenu=""):
    """Print the menu entry to upgrade all outdated package of a manager."""
    if manager.get('upgrade_all_cli'):
        if not FLAT_LAYOUT:
            echo("-----")
        echo(
            "{}Upgrade all | {} terminal=false refresh=true {f_normal}".format(
                submenu, manager['upgrade_all_cli'], f_normal=FONTS['normal']))


def print_menu():
    """Print menu structure using BitBar's plugin API.

    See: https://github.com/matryer/bitbar#plugin-api
    """
    # Search for generic mpm CLI on system.
    code, _, error = run('mpm')
    # mpm CLI hasn't been found on the system. Propose to the user to install
    # or upgrade it.
    if code or error:
        print_error_header()
        print_error(error)
        echo("---")
        echo(
            "Install / upgrade `mpm` CLI. | bash=pip param1=install "
            # TODO: Add minimal requirement on Python package.
            "param2=--upgrade param3=meta-package-manager terminal=true "
            "refresh=true {f_error}".format(error, f_error=FONTS['error']))
        return

    # Fetch list of all outdated packages from all package manager available on
    # the system.
    _, output, error = run(
        'mpm', '--output-format', 'json', 'outdated', '--cli-format', 'bitbar')

    # Bail-out immediately on errors related to mpm self-execution or if mpm is
    # not able to produce any output.
    if error or not output:
        print_error_header()
        print_error(error)
        return

    # Sort outdated packages by manager's name.
    managers = sorted(json.loads(output).values(), key=itemgetter('name'))

    # Print menu bar icon with number of available upgrades.
    total_outdated = sum([len(m['packages']) for m in managers])
    total_errors = len([True for m in managers if m.get('error', None)])
    echo("↑{}{} | dropdown=false".format(
        total_outdated,
        " ⚠️{}".format(total_errors) if total_errors else ""))

    # Print a full detailed section for each manager.
    submenu = "--" if not FLAT_LAYOUT else ""

    if not FLAT_LAYOUT:
        # Compute maximal manager's name length.
        label_max_length = max([len(m['name']) for m in managers])
        max_outdated = max([len(m['packages']) for m in managers])

    if not FLAT_LAYOUT:
        echo("---")

    for manager in managers:
        if FLAT_LAYOUT:
            echo("---")

        package_label = "package{}".format(
            's' if len(manager['packages']) != 1 else '')

        if FLAT_LAYOUT:
            echo("{0} outdated {1} {2} | {f_summary} emojize=false".format(
                len(manager['packages']),
                manager['name'],
                package_label,
                f_summary=FONTS['summary']))

        else:
            # Non-flat layout use a compact table-like rendering of manager
            # summary.
            echo(
                "{error}{0:<{max_length}} {1:>{max_outdated}} {2:<8} | "
                "{f_summary} emojize=false".format(
                    manager['name'] + ':',
                    len(manager['packages']),
                    package_label,
                    error="⚠️ " if manager.get('error', None) else '',
                    max_length=label_max_length + 1,
                    max_outdated=len(str(max_outdated)),
                    f_summary=FONTS['summary']))

        print_package_items(manager['packages'], submenu)

        print_upgrade_all_item(manager, submenu)

        if manager.get('error', None):
            echo("---" if FLAT_LAYOUT else "-----")
            print_error(manager['error'], submenu)


if __name__ == '__main__':
    fix_environment()
    print_menu()
