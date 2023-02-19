#!/usr/bin/env python3
# <xbar.title>Meta Package Manager</xbar.title>
# <xbar.version>v5.8.0</xbar.version>
# <xbar.author>Kevin Deldycke</xbar.author>
# <xbar.author.github>kdeldycke</xbar.author.github>
# <xbar.desc>List outdated packages and manage upgrades.</xbar.desc>
# <xbar.dependencies>python,mpm</xbar.dependencies>
# <xbar.image>https://i.imgur.com/B5wdxIc.png</xbar.image>
# <xbar.abouturl>https://github.com/kdeldycke/meta-package-manager</xbar.abouturl>
# <xbar.var>boolean(VAR_SUBMENU_LAYOUT=false): Group packages into a sub-menu for each manager.</xbar.var>
# <xbar.var>boolean(VAR_TABLE_RENDERING=true): Aligns package names and versions in a table for easier visual parsing.</xbar.var>
# XXX Deactivate font-related options for Xbar. Default variable value does not allow `=` character in Xbar. See: https://github.com/matryer/xbar/issues/832
# <!--xbar.var>string(VAR_DEFAULT_FONT=""): Default font to use for non-monospaced text.</xbar.var-->
# <!--xbar.var>string(VAR_MONOSPACE_FONT="font=Menlo size=12"): Default configuration for monospace fonts, including errors. Is used for table rendering.</xbar.var-->
# <swiftbar.environment>[VAR_SUBMENU_LAYOUT: false, VAR_TABLE_RENDERING: true, VAR_DEFAULT_FONT: , VAR_MONOSPACE_FONT: font=Menlo size=12]</swiftbar.environment>

"""Xbar and SwiftBar plugin for Meta Package Manager (i.e. the :command:`mpm` CLI).

Default update cycle is set to 7 hours so we have a chance to get user's
attention once a day. Higher frequency might ruin the system as all checks are
quite resource intensive, and Homebrew might hit GitHub's API calls quota.

- Xbar automatically bridge plugin options between its UI and environment
  variable on script execution. See:
  https://xbarapp.com/docs/2021/03/14/variables-in-xbar.html

- This is in progress for SwiftBar at:
  https://github.com/swiftbar/SwiftBar/issues/160
"""
from __future__ import annotations

import sys

python_min_version = (3, 7, 3)
"""Minimal requirement is macOS Catalina (10.15) for both Xbar and SwiftBar.

Catalina deprecates Python 2.x, and ships with Python 3.7.3. So this plugin is
required to work with Python 3.7.3 or newer.
"""


def v_to_str(version_tuple: tuple[int, ...] | None) -> str:
    """Transforms into a string a tuple of integers representing a version."""
    if not version_tuple:
        return "None"
    return ".".join(map(str, version_tuple))


if sys.version_info < python_min_version:
    raise SystemError(
        f"Bar plugin ran with Python {sys.version}, "
        f"but requires Python >= {v_to_str(python_min_version)}"
    )

import argparse
import os
import re
from configparser import RawConfigParser
from shutil import which
from subprocess import run
from unittest.mock import patch

if sys.version_info >= (3, 8):
    from functools import cached_property
else:
    cached_property = property


class MPMPlugin:
    """Implements the minimal code necessary to locate and call the ``mpm`` CLI on the
    system.

    Once ``mpm`` is located, we can rely on it to produce the main output of the plugin.

    The output must supports both Xbar and SwiftBar:
        - https://github.com/matryer/xbar-plugins/blob/main/CONTRIBUTING.md#plugin-api
        - https://github.com/swiftbar/SwiftBar#plugin-api
    """

    mpm_min_version = (5, 0, 0)
    """Mpm v5.0.0 was the first version taking care of the complete layout rendering."""

    @staticmethod
    def extended_environment() -> dict[str, str]:
        """Returns a tweaked environment extending global path to find non-default
        system-wide binaries.

        macOS does not put ``/usr/local/bin`` or ``/opt/local/bin`` in the ``PATH`` for
        GUI apps. For some package managers this is a problem. Additionally Homebrew and
        Macports are using different paths. So, to make sure we can always get to the
        necessary binaries, we overload the path. Current preference order would equate
        to Homebrew, Macports, then system.
        """
        # Cast to dict to make a copy and prevent modification of the global
        # environment.
        env_copy = dict(os.environ)
        env_copy["PATH"] = ":".join(
            (
                # Homebrew Apple silicon.
                "/opt/homebrew/bin",
                "/opt/homebrew/sbin",
                # Homebrew Intel.
                "/usr/local/bin",
                "/usr/local/sbin",
                # Macports.
                "/opt/local/bin",
                "/opt/local/sbin",
                # System.
                os.environ.get("PATH", ""),
            )
        )
        return env_copy

    @staticmethod
    def getenv_str(var, default: str | None = None) -> str | None:
        """Utility to get environment variables.

        Note that all environment variables are strings. Always returns a lowered-case
        string.
        """
        value = os.environ.get(var, None)
        if value is None:
            return default
        return str(value).lower()

    @staticmethod
    def getenv_bool(var, default: bool = False) -> bool:
        """Utility to normalize boolean environment variables.

        Relies on ``configparser.RawConfigParser.BOOLEAN_STATES`` to translate strings into boolean. See:
        https://github.com/python/cpython/blob/89192c46da7b984811ff3bd648f8e827e4ef053c/Lib/configparser.py#L597-L599
        """
        value = MPMPlugin.getenv_str(var)
        if value is None:
            return default
        return RawConfigParser.BOOLEAN_STATES[value]

    @staticmethod
    def normalize_params(
        font_string: str, valid_ids: set[str] = {"color", "font", "size"}
    ) -> str:
        valid_params = {}
        for param in font_string.split():
            param_id, param_value = param.split("=", 1)
            if param_id in valid_ids:
                valid_params[param_id] = param_value
        return " ".join(f"{k}={v}" for k, v in valid_params.items())

    @cached_property
    def table_rendering(self) -> bool:
        """Aligns package names and versions, like a table, for easier visual parsing.

        If ``True``, will aligns all items using a fixed-width font.
        """
        return self.getenv_bool("VAR_TABLE_RENDERING", True)

    @cached_property
    def default_font(self) -> str:
        """Make it easier to change font, sizes and colors of the output."""
        return self.normalize_params(
            self.getenv_str("VAR_DEFAULT_FONT", "")  # type: ignore
        )

    @cached_property
    def monospace_font(self) -> str:
        """Make it easier to change font, sizes and colors of the output."""
        return self.normalize_params(
            self.getenv_str("VAR_MONOSPACE_FONT", "font=Menlo size=12")  # type: ignore
        )

    @cached_property
    def error_font(self) -> str:
        """Error font is based on monospace font."""
        return self.normalize_params(f"{self.monospace_font} color=red")

    @cached_property
    def is_swiftbar(self) -> bool:
        """SwiftBar is kind enough to tell us about its presence."""
        return self.getenv_bool("SWIFTBAR")

    @staticmethod
    def locate_bin(*bin_names: str) -> str | None:
        """Find the location of an executable binary on the system.

        Provides as many binary names as you need, the first one found will be returned.
        Both plain name and full path are supported.
        """
        for name in bin_names:
            path = which(name)
            if path:
                return path
        return None

    @cached_property
    def python_path(self) -> str:
        """Returns the system's Python binary path.

        This plugin being run from Python, we have the one called by Xbar/SwiftBar to
        fallback to (i.e. ``sys.executable``). But before that, we attempt to locate it
        by respecting the environment variables.
        """
        return self.locate_bin("python", "python3", sys.executable)  # type: ignore

    @cached_property
    def mpm_exec(self) -> tuple[str, ...]:
        """Search for mpm execution alternatives, either direct ``mpm`` call or as an
        executable Python module."""
        # XXX Local debugging and development.
        # return "poetry", "run", "mpm"
        mpm_exec = self.locate_bin("mpm")
        if mpm_exec:
            return (mpm_exec,)
        return self.python_path, "-m", "meta_package_manager"

    def check_mpm(
        self,
    ) -> tuple[bool, tuple[int, ...] | None, bool, str | Exception | None]:
        """Test-run mpm execution and extract its version."""
        error: str | Exception | None = None
        try:
            process = run(
                (*self.mpm_exec, "--version"), capture_output=True, encoding="utf-8"
            )
            error = process.stderr
        except FileNotFoundError as ex:
            error = ex

        installed = False
        mpm_version = None
        up_to_date = False
        # Is mpm CLI installed on the system?
        if not process.returncode and not error:
            installed = True
            # Is mpm too old?
            match = re.compile(r".*\s+(?P<version>[0-9\.]+)$", re.MULTILINE).search(
                process.stdout
            )
            if match:
                version_string = match.groupdict()["version"]
                mpm_version = tuple(map(int, version_string.split(".")))
                if mpm_version >= self.mpm_min_version:
                    up_to_date = True

        return installed, mpm_version, up_to_date, error

    @staticmethod
    def pp(*args: str) -> None:
        """Print one menu-line with the Xbar/SwiftBar dialect.

        First argument is the menu-line label, separated by a pipe to all other non-
        empty parameters, themselves separated by a space.
        """
        line: list[str] = []
        for param in args:
            if param:
                if len(line) == 1:
                    line.append("|")
                line.append(param)
        print(*line, sep=" ")

    @staticmethod
    def print_error_header() -> None:
        """Generic header for blocking error."""
        MPMPlugin.pp("❗️", "dropdown=false")
        print("---")

    def print_error(self, message: str | Exception, submenu: str = "") -> None:
        """Print a formatted error line by line.

        A red, fixed-width font is used to preserve traceback and exception layout.
        """
        # Cast to string as we might directly pass exceptions for rendering.
        for line in str(message).strip().splitlines():
            self.pp(
                f"{submenu}{line}",
                self.error_font,
                "trim=false",
                "ansi=false",
                "emojize=false",
                "symbolize=false" if self.is_swiftbar else "",
            )

    def print_menu(self) -> None:
        """Print the main menu."""
        mpm_installed, _, mpm_up_to_date, error = self.check_mpm()
        if not mpm_installed or not mpm_up_to_date:
            self.print_error_header()
            if error:
                self.print_error(error)
                print("---")
            action_msg = "Install" if not mpm_installed else "Upgrade"
            min_version_str = v_to_str(self.mpm_min_version)
            self.pp(
                f"{action_msg} mpm >= v{min_version_str}",
                f"shell={self.python_path}",
                "param1=-m",
                "param2=pip",
                "param3=install",
                "param4=--upgrade",
                # XXX This seems broken beyond repair. No amount of workaround works. See:
                # https://github.com/matryer/xbar/issues/831
                # https://github.com/swiftbar/SwiftBar/issues/308
                # Fallback to the only version that is working on SwiftBar.
                f'param5=\\"meta-package-manager>={min_version_str}\\"',
                self.error_font,
                "refresh=true",
                "terminal=true",
            )
            return

        # Force a sync of all local package databases.
        run((*self.mpm_exec, "--verbosity", "ERROR", "sync"))

        # Fetch outdated packages from all package managers available on the system.
        # We defer all rendering to mpm itself so it can compute more intricate layouts.
        process = run(
            # We silence all errors but the CRITICAL ones. All others will be captured
            # by mpm in --plugin-output mode and rendered back into each manager
            # section.
            (*self.mpm_exec, "--verbosity", "CRITICAL", "outdated", "--plugin-output"),
            capture_output=True,
            encoding="utf-8",
        )

        # Bail-out immediately on errors related to mpm self-execution or if mpm is
        # not able to produce any output.
        if process.stderr or not process.stdout:
            self.print_error_header()
            self.print_error(process.stderr)
        else:
            # Capturing the output of mpm and re-printing it will introduce an extra
            # line returns, hence the extra rstrip() call.
            print(process.stdout.rstrip())


if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--check-mpm",
        action="store_true",
        help="Locate mpm on the system and check its version.",
    )
    args = parser.parse_args()

    # Wrap plugin execution with our custom environment variables to avoid
    # environment leaks.
    with patch.dict("os.environ", MPMPlugin.extended_environment()):
        plugin = MPMPlugin()

        if args.check_mpm:
            mpm_installed, mpm_version, mpm_up_to_date, error = plugin.check_mpm()
            if not mpm_installed:
                raise FileNotFoundError(error)
            if not mpm_up_to_date:
                raise ValueError(
                    f"{plugin.mpm_exec} is too old: "
                    f"{v_to_str(mpm_version)} < {v_to_str(plugin.mpm_min_version)}"
                )
            print(f"{' '.join(plugin.mpm_exec)} v{v_to_str(mpm_version)}")

        else:
            plugin.print_menu()
