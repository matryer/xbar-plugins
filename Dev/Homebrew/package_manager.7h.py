#!/usr/bin/env python
# -*- coding: utf-8 -*-
# <bitbar.title>Package Manager</bitbar.title>
# <bitbar.version>v1.6</bitbar.version>
# <bitbar.author>Kevin Deldycke</bitbar.author>
# <bitbar.author.github>kdeldycke</bitbar.author.github>
# <bitbar.desc>List package updates available from Homebrew, Cask, Python's pip2 and pip3, Node's npm, Atom's apm, Rebuy's gem and Mac AppStore via mas CLI. Allows individual or full upgrades (if available).</bitbar.desc>
# <bitbar.dependencies>python,homebrew,cask,pip,npm,apm,gem,mas</bitbar.dependencies>
# <bitbar.image>https://i.imgur.com/CiQpQ42.png</bitbar.image>
# <bitbar.abouturl>https://github.com/kdeldycke/dotfiles/blob/master/dotfiles-osx/.bitbar/package_manager.7h.py</bitbar.abouturl>

"""
Default update cycle is set to 7 hours so we have a chance to get user's
attention once a day. Higher frequency might ruin the system as all checks are
quite resource intensive, and Homebrew might hit GitHub's API calls quota.

If you're bored, feel free to add support for new package manager. A list of
good candidates is available at:
https://en.wikipedia.org/wiki/List_of_software_package_management_systems


Changelog
=========

1.6 (2016-08-10)
----------------

* Work around the lacks of full pip upgrade command.
* Fix UnicodeDecodeError on parsing CLI output.

1.5 (2016-07-25)
----------------

* Add support for [mas](https://github.com/argon/mas).
* Don't show all stderr as err (check return code for error state).

1.4 (2016-07-10)
----------------

* Don't attempt to parse empty lines.
* Check for linked npm packages.
* Support System or Homebrew Ruby Gems (with proper sudo setup).

1.3 (2016-07-09)
----------------

* Add changelog.
* Add reference to package manager's issues.
* Force Cask update before evaluating available packages.
* Add sample of command output as version parsing can be tricky.

1.2 (2016-07-08)
----------------

* Add support for both pip2 and pip3, Node's npm, Atom's apm, Ruby's gem.
  Thanks @tresni.
* Fixup brew cask checking. Thanks @tresni.
* Don't die on errors. Thanks @tresni.

1.1 (2016-07-07)
----------------

* Add support for Python's pip.

1.0 (2016-07-05)
----------------

* Initial public release.
* Add support for Homebrew and Cask.

0.0 (2016-07-05)
-----------------

* First commit.
"""

from __future__ import print_function, unicode_literals

import json
import os
import re
from operator import itemgetter, methodcaller
from subprocess import PIPE, Popen


class PackageManager(object):
    """ Generic class for a package manager. """

    cli = None

    def __init__(self):
        # List all available updates and their versions.
        self.updates = []
        self.error = None

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

    def run(self, *args):
        """ Run a shell command, return the output and keep error message.
        """
        self.error = None
        process = Popen(
            args, stdout=PIPE, stderr=PIPE, universal_newlines=True)
        output, error = process.communicate()
        if process.returncode != 0 and error:
            self.error = error.decode('utf-8')
        return output.decode('utf-8')

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
        """ Fetch latest Homebrew formulas.

        Sample of brew output:

            $ brew outdated --json=v1
            [
              {
                "name": "cassandra",
                "installed_versions": [
                  "3.5"
                ],
                "current_version": "3.7"
              },
              {
                "name": "vim",
                "installed_versions": [
                  "7.4.1967"
                ],
                "current_version": "7.4.1993"
              },
              {
                "name": "youtube-dl",
                "installed_versions": [
                  "2016.07.06"
                ],
                "current_version": "2016.07.09.1"
              }
            ]
        """
        self.run(self.cli, 'update')

        # List available updates.
        output = self.run(self.cli, 'outdated', '--json=v1')
        if not output:
            return

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

    @property
    def active(self):
        """ Cask depends on vanilla Homebrew. """
        if super(Cask, self).active:
            cask = Popen([self.cli, 'cask'], stdout=PIPE, stderr=PIPE)
            cask.communicate()
            return cask.returncode == 0
        return False

    def sync(self):
        """ Fetch latest formulas and their metadata.

        Sample of brew cask output:

            $ brew cask list --versions
            aerial 1.2beta5, 1.1
            android-file-transfer latest
            audacity 2.1.2-1453294898, 2.1.2
            bitbar 1.9.1
            chromium latest
            firefox 47.0, 46.0.1, 46.0
            flux 37.3, 37.2, 37.1, 36.8, 36.6
            gimp 2.8.16-x86_64
            java 1.8.0_92-b14
            prey
            ubersicht

            $ brew cask info aerial
            aerial: 1.2beta5
            Aerial Screensaver
            https://github.com/JohnCoates/Aerial
            /usr/local/Caskroom/aerial/1.2beta5 (0B)
            https://github.com/caskroom/homebrew-cask/blob/master/Casks/aerial.rb
            ==> Contents
              Aerial.saver (screen_saver)

            $ brew cask info firefox
            firefox: 47.0.1
            Mozilla Firefox
            https://www.mozilla.org/en-US/firefox/
            Not installed
            https://github.com/caskroom/homebrew-cask/blob/master/Casks/firefox.rb
            ==> Contents
              Firefox.app (app)

            $ brew cask info prey
            prey: 1.5.1
            Prey
            https://preyproject.com
            Not installed
            https://github.com/caskroom/homebrew-cask/blob/master/Casks/prey.rb
            ==> Contents
              prey-mac-1.5.1-x86.pkg (pkg)

            $ brew cask info ubersicht
            ubersicht: 1.0.42
            Übersicht
            http://tracesof.net/uebersicht
            Not installed
            https://github.com/caskroom/homebrew-cask/blob/master/Casks/ubersicht.rb
            ==> Contents
              Übersicht.app (app)
        """
        # `brew cask update` is just an alias to `brew update`. Perform the
        # action anyway to make it future proof.
        self.run(self.cli, 'cask', 'update')

        # List installed packages.
        output = self.run(self.cli, 'cask', 'list', '--versions')

        # Inspect package one by one as `brew cask list` is not reliable. See:
        # https://github.com/caskroom/homebrew-cask/blob/master/doc
        # /reporting_bugs/brew_cask_list_shows_wrong_information.md
        for installed_pkg in output.strip().split('\n'):
            if not installed_pkg:
                continue
            name, versions = installed_pkg.split(' ', 1)

            # Use heuristics to guess installed version.
            versions = sorted([
                v.strip() for v in versions.split(',') if v.strip()])
            if len(versions) > 1 and 'latest' in versions:
                versions.remove('latest')
            version = versions[-1] if versions else '?'

            # TODO: Support packages removed from repository (reported with a
            # `(!)` flag). See: https://github.com/caskroom/homebrew-cask/blob
            # /master/doc/reporting_bugs
            # /uninstall_wrongly_reports_cask_as_not_installed.md

            # Inspect the package closer to evaluate its state.
            output = self.run(self.cli, 'cask', 'info', name)

            # Consider package as up-to-date if installed.
            if output.find('Not installed') == -1:
                continue

            latest_version = output.split('\n')[0].split(' ')[1]

            self.updates.append({
                'name': name,
                'installed_version': version,
                'latest_version': latest_version})

    def update_cli(self, package_name):
        """ Install a package.

        TODO: wait for https://github.com/caskroom/homebrew-cask/issues/22647
        so we can force a cleanup in one go, as we do above with vanilla
        Homebrew.
        """
        return self.bitbar_cli_format(
            "{} cask install {}".format(self.cli, package_name))

    def update_all_cli(self):
        """ Cask has no way to update all outdated packages.

        See: https://github.com/caskroom/homebrew-cask/issues/4678
        """
        return


class Pip(PackageManager):

    def sync(self):
        """ List outdated packages and their metadata.

        Sample of pip output:

            $ pip list --outdated
            ccm (2.1.8, /Users/kdeldycke/ccm) - Latest: 2.1.11 [sdist]
            coverage (4.0.3) - Latest: 4.1 [wheel]
            IMAPClient (0.13) - Latest: 1.0.1 [wheel]
            Logbook (0.10.1) - Latest: 1.0.0 [sdist]
            mccabe (0.4.0) - Latest: 0.5.0 [wheel]
            mercurial (3.8.3) - Latest: 3.8.4 [sdist]
            pylint (1.5.6) - Latest: 1.6.1 [wheel]
        """
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
        """ Produce a long CLI with all upgradeable package names.

        This work around the lack of proper full upgrade command in Pip.
        See: https://github.com/pypa/pip/issues/59
        """
        return self.update_cli(' '.join(map(itemgetter('name'), self.updates)))


class Pip2(Pip):

    cli = '/usr/local/bin/pip2'

    @property
    def name(self):
        return "Python 2 pip"


class Pip3(Pip):

    cli = '/usr/local/bin/pip3'

    @property
    def name(self):
        return "Python 3 pip"


class NPM(PackageManager):

    cli = '/usr/local/bin/npm'

    @property
    def name(self):
        return "npm"

    def sync(self):
        """
        Sample of npm output:

            $ npm -g --progress=false --json outdated
            {
              "my-linked-package": {
                "current": "0.0.0-development",
                "wanted": "linked",
                "latest": "linked",
                "location": "/Users/..."
              },
              "npm": {
                "current": "3.10.3",
                "wanted": "3.10.5",
                "latest": "3.10.5",
                "location": "/Users/..."
              }
            }
        """
        output = self.run(
            self.cli, '-g', '--progress=false', '--json', 'outdated')
        if not output:
            return

        for package, values in json.loads(output).iteritems():
            if values['wanted'] == 'linked':
                continue
            self.updates.append({
                'name': package,
                'installed_version': values['current'],
                'latest_version': values['latest']
            })

    def update_cli(self, package_name=None):
        cmd = "{} -g --progress=false update".format(self.cli)
        if package_name:
            cmd += " {}".format(package_name)
        return self.bitbar_cli_format(cmd)

    def update_all_cli(self):
        return self.update_cli()


class APM(PackageManager):

    cli = '/usr/local/bin/apm'

    @property
    def name(self):
        return "apm"

    def sync(self):
        output = self.run(self.cli, 'outdated', '--compatible', '--json')
        if not output:
            return

        for package in json.loads(output):
            self.updates.append({
                'name': package['name'],
                'installed_version': package['version'],
                'latest_version': package['latestVersion']
            })

    def update_cli(self, package_name=None):
        cmd = "{} update --no-confirm".format(self.cli)
        if package_name:
            cmd += " {}".format(package_name)
        return self.bitbar_cli_format(cmd)

    def update_all_cli(self):
        return self.update_cli()


class Gems(PackageManager):
    HOMEBREW_PATH = '/usr/local/bin/gem'
    SYSTEM_PATH = '/usr/bin/gem'

    def __init__(self):
        super(Gems, self).__init__()

        self.system = True
        if os.path.exists(Gems.HOMEBREW_PATH):
            self.system = False
            self._cli = Gems.HOMEBREW_PATH
        else:
            self._cli = Gems.SYSTEM_PATH

    @property
    def cli(self):
        return self._cli

    @property
    def name(self):
        return "Ruby Gems"

    def sync(self):
        """
        Sample of gem output:

            $ gem outdated
            did_you_mean (1.0.0 < 1.0.2)
            io-console (0.4.5 < 0.4.6)
            json (1.8.3 < 2.0.1)
            minitest (5.8.3 < 5.9.0)
            power_assert (0.2.6 < 0.3.0)
            psych (2.0.17 < 2.1.0)
        """
        # outdated does not require sudo privileges on homebrew or system
        output = self.run(self.cli, 'outdated')

        regexp = re.compile(r'(\S+) \((\S+) < (\S+)\)')
        for package in output.split('\n'):
            if not package:
                continue
            name, current_version, latest_version = regexp.match(
                package).groups()
            self.updates.append({
                'name': name,
                'installed_version': current_version,
                'latest_version': latest_version
            })

    def update_cli(self, package_name=None):
        # installs require sudo on system ruby
        cmd = "{}{} update".format(
            '/usr/bin/sudo ' if self.system else '',
            self.cli)
        if package_name:
            cmd += " {}".format(package_name)
        return self.bitbar_cli_format(cmd)

    def update_all_cli(self):
        return self.update_cli()


class MAS(PackageManager):

    cli = '/usr/local/bin/mas'

    def __init__(self):
        super(MAS, self).__init__()
        self.map = {}

    @property
    def name(self):
        return "Mac AppStore"

    def sync(self):
        output = self.run(self.cli, 'outdated')
        if not output:
            return

        regexp = re.compile(r'(\d+) (.*) \((\S+)\)$')
        for application in output.split('\n'):
            if not application:
                continue
            _id, name, version = regexp.match(application).groups()
            self.map[name] = _id
            self.updates.append({
                'name': name,
                'latest_version': version,
                'installed_version': ''
            })

    def update_cli(self, package_name):
        if package_name not in self.map:
            return None
        cmd = "{} install {}".format(self.cli, self.map[package_name])
        return self.bitbar_cli_format(cmd)

    def update_all_cli(self):
        cmd = "{} upgrade".format(self.cli)
        return self.bitbar_cli_format(cmd)


def print_menu():
    """ Print menu structure using BitBar's plugin API.

    See: https://github.com/matryer/bitbar#plugin-api
    """
    # Instantiate all available package manager.
    managers = [k() for k in [Homebrew, Cask, Pip2, Pip3, APM, NPM, Gems, MAS]]

    # Filters-out inactive managers.
    managers = [m for m in managers if m.active]

    # Sync all managers.
    map(methodcaller('sync'), managers)

    # Print menu bar icon with number of available updates.
    total_updates = sum([len(m.updates) for m in managers])
    errors = [True for m in managers if m.error]
    print(("↑{} {}| dropdown=false".format(
        total_updates,
        "⚠️{}".format(len(errors)) if errors else ""
    )).encode('utf-8'))

    # Print a full detailed section for each manager.
    for manager in managers:
        print("---")

        if manager.error:
            for line in manager.error.strip().split("\n"):
                print("{} | color=red".format(line))

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
