#!/usr/bin/env python
# -*- coding: utf-8 -*-
# <bitbar.title>Package Manager</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Kevin Deldycke</bitbar.author>
# <bitbar.author.github>kdeldycke</bitbar.author.github>
# <bitbar.desc>List package updates available from Homebrew, Cask and Pip. Allows individual or full upgrades (if available).</bitbar.desc>
# <bitbar.dependencies>python,homebrew,cask,pip</bitbar.dependencies>
# <bitbar.image>https://i.imgur.com/oXL2Nyn.png</bitbar.image>
# <bitbar.abouturl>https://github.com/kdeldycke/dotfiles/blob/master/dotfiles-osx/.bitbar/package_manager.7h.py</bitbar.abouturl>

from __future__ import print_function, unicode_literals

from subprocess import Popen, PIPE
import sys
import json
import os
from operator import methodcaller
import re


# TODO: add cleanup commands.


class PackageManager(object):
    """ Generic class for a package manager. """

    def __init__(self):
        # List all available updates and their versions.
        self.updates = []

    @property
    def name(self):
        """ Return package manager's common name. Defaults based on class name.
        """
        return self.__class__.__name__

    @property
    def active(self):
        """ Is the package manager available on the system?

        Returns True is the main CLI exists and is executable.
        """
        return os.path.isfile(self.cli) and os.access(self.cli, os.X_OK)

    @staticmethod
    def run(*args):
        """ Run a shell command, and exits right away on error. """
        output, error = Popen(args, stdout=PIPE).communicate()
        if error:
            print("Error | color=red")
            sys.exit(error)
        return output

    def sync(self):
        """ Fetch latest versions of installed packages.

        Returns a list of dict with package name, current installed version and
        latest upgradeable version.
        """
        raise NotImplementedError

    @staticmethod
    def bitbar_cli_format(full_cli):
        """ Format a bash-runnable full-CLI with parameters into bitbar schema.
        """
        cmd, params = full_cli.strip().split(' ', 1)
        bitbar_cli = "bash={}".format(cmd)
        for index, param in enumerate(params.split(' ')):
            bitbar_cli += " param{}={}".format(index + 1, param)
        return bitbar_cli

    def update_cli(self, package_name):
        """ Return a bitbar-compatible full-CLI to update a package. """
        raise NotImplementedError

    def update_all_cli(self):
        """ Return a bitbar-compatible full-CLI to update all packages. """
        raise NotImplementedError


class Homebrew(PackageManager):

    cli = '/usr/local/bin/brew'

    def sync(self):
        """ Fetch latest Homebrew formulas. """
        self.run(self.cli, 'update')

        # List available updates.
        output = self.run(self.cli, 'outdated', '--json=v1')

        for pkg_info in json.loads(output):
            self.updates.append({
                'name': pkg_info['name'],
                # Only keeps the highest installed version.
                'installed_version': max(pkg_info['installed_versions']),
                'latest_version': pkg_info['current_version']})

    def update_cli(self, package_name=None):
        cmd = "{} upgrade --cleanup".format(self.cli)
        if package_name:
            cmd += " {}".format(package_name)
        return self.bitbar_cli_format(cmd)

    def update_all_cli(self):
        return self.update_cli()


class Cask(Homebrew):

    def sync(self):
        """ Fetch latest formulas and their metadata. """
        # No need to update formulas if Homebrew is synced first.

        # List installed packages.
        output = self.run(self.cli, 'cask', 'list', '--versions')

        for installed_pkg in output.strip().split('\n'):
            name, versions = installed_pkg.split(' ', 1)

            # `brew cask list` is broken. Use heuristics to guess the currently
            # installed version.
            # See: https://github.com/caskroom/homebrew-cask/issues/14058
            versions = sorted([
                v.strip() for v in versions.split(',') if v.strip()])
            if len(versions) > 1 and 'latest' in versions:
                versions.remove('latest')
            version = versions[-1] if versions else '?'

            # Look closer to the package to guess its state.
            output = self.run(self.cli, 'cask', 'info', name)

            # Package is up-to-date.
            if output.find('Not installed') == -1:
                continue

            latest_version = output.split('\n')[0].split(' ')[1]

            self.updates.append({
                'name': name,
                'installed_version': version,
                'latest_version': latest_version})

    def update_cli(self, package_name):
        return self.bitbar_cli_format(
            "{} cask install {}".format(self.cli, package_name))

    def update_all_cli(self):
        """ Cask has no way to update all outdated packages. """
        return


class Pip(PackageManager):

    cli = '/usr/local/bin/pip'

    def sync(self):
        """ List outdated packages and their metadata. """
        output = self.run(self.cli, 'list', '--outdated').strip()
        if not output:
            return

        regexp = re.compile(r'(\S+) \((.*)\) - Latest: (\S+)')
        for outdated_pkg in output.split('\n'):
            name, installed_info, latest_version = regexp.match(
                outdated_pkg).groups()

            # Extract current non-standard location if found.
            installed_info = installed_info.split(',', 1)
            version = installed_info[0]
            special_location = " ({})".format(
                installed_info[1].strip()) if len(installed_info) > 1 else ''

            self.updates.append({
                'name': name + special_location,
                'installed_version': version,
                'latest_version': latest_version})

    def update_cli(self, package_name):
        return self.bitbar_cli_format(
            "{} install --upgrade {}".format(self.cli, package_name))

    def update_all_cli(self):
        """ Pip doesn't support full upgrade yet. """
        return


def print_menu():
    """ Print menu structure using BitBar's plugin API.

    See: https://github.com/matryer/bitbar#plugin-api
    """
    # Instantiate all available package manager.
    managers = [k() for k in [Homebrew, Cask, Pip]]

    # Filters-out inactive managers.
    managers = [m for m in managers if m.active]

    # Sync all managers.
    map(methodcaller('sync'), managers)

    # Print menu bar icon with number of available updates.
    total_updates = sum([len(m.updates) for m in managers])
    print(("↑{} | dropdown=false".format(total_updates)).encode('utf-8'))

    # Print a full detailed section for each manager.
    for manager in managers:
        print("---")

        print("{} {} package{}".format(
            len(manager.updates),
            manager.name,
            's' if len(manager.updates) > 1 else ''))

        if manager.update_all_cli() and manager.updates:
            print("Upgrade all | {} terminal=false refresh=true".format(
                manager.update_all_cli()))

        for pkg_info in manager.updates:
            print((
                "{name} {installed_version} → {latest_version} | "
                "{cli} terminal=false refresh=true".format(
                    cli=manager.update_cli(pkg_info['name']),
                    **pkg_info)).encode('utf-8'))

print_menu()
