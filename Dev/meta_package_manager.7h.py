#!/usr/bin/env python3
# <xbar.title>Meta Package Manager</xbar.title>
# <xbar.version>v5.14.1</xbar.version>
# <xbar.author>Kevin Deldycke</xbar.author>
# <xbar.author.github>kdeldycke</xbar.author.github>
# <xbar.desc>List outdated packages and manage upgrades.</xbar.desc>
# <xbar.dependencies>python,mpm</xbar.dependencies>
# <xbar.image>https://i.imgur.com/B5wdxIc.png</xbar.image>
# <xbar.abouturl>
#    https://kdeldycke.github.io/meta-package-manager/bar-plugin.html
# </xbar.abouturl>
# <xbar.var>
#   boolean(VAR_SUBMENU_LAYOUT=false):
#   Group packages into a sub-menu for each manager.
# </xbar.var>
# <xbar.var>
#   boolean(VAR_TABLE_RENDERING=true):
#   Aligns package names and versions in a table for easier visual parsing.
# </xbar.var>
#
# XXX Deactivate font-related options for Xbar. Default variable value does not allow
# XXX `=` character in Xbar. See: https://github.com/matryer/xbar/issues/832
# <!--xbar.var>
#   string(VAR_DEFAULT_FONT=""):
#   Default font to use for non-monospaced text.
# </xbar.var-->
# <!--xbar.var>
#   string(VAR_MONOSPACE_FONT="font=Menlo size=12"):
#   Default configuration for monospace fonts, including errors.
#   Is used for table rendering.
# </xbar.var-->
# <swiftbar.environment>
#   [
#       VAR_SUBMENU_LAYOUT: false,
#       VAR_TABLE_RENDERING: true,
#       VAR_DEFAULT_FONT: ,
#       VAR_MONOSPACE_FONT: font=Menlo size=12
#   ]
# </swiftbar.environment>
"""Xbar and SwiftBar plugin for Meta Package Manager (i.e. the :command:`mpm` CLI).

Default update cycle should be set to several hours so we have a chance to get
user's attention once a day. Higher frequency might ruin the system as all
checks are quite resource intensive, and Homebrew might hit GitHub's API calls
quota.

- `Xbar automatically bridge plugin options
  <https://xbarapp.com/docs/2021/03/14/variables-in-xbar.html>`_ between its UI
  and environment variable on script execution.

- This is `in progress for SwiftBar
  <https://github.com/swiftbar/SwiftBar/issues/160>`_.
"""

from __future__ import annotations

import argparse
import os
import re
import sys
from configparser import RawConfigParser
from functools import cached_property
from operator import methodcaller
from shutil import which
from subprocess import run
from textwrap import dedent
from unittest.mock import patch

python_min_version = (3, 8, 0)
"""Minimal requirement is aligned to mpm."""


def v_to_str(version_tuple: tuple[int, ...] | None) -> str:
    """Transforms into a string a tuple of integers representing a version."""
    if not version_tuple:
        return "None"
    return ".".join(map(str, version_tuple))


if sys.version_info < python_min_version:
    msg = (
        f"Bar plugin ran with Python {sys.version}, but requires "
        f"Python >= {v_to_str(python_min_version)}"
    )
    raise SystemError(msg)


class MPMPlugin:
    """Implements the minimal code necessary to locate and call the ``mpm`` CLI on the
    system.

    Once ``mpm`` is located, we can rely on it to produce the main output of the plugin.

    The output must supports both `Xbar dialect
    <https://github.com/matryer/xbar-plugins/blob/main/CONTRIBUTING.md#plugin-api>`_
    and `SwiftBar dialect <https://github.com/swiftbar/SwiftBar#plugin-api>`_.
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
            ),
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

        Relies on ``configparser.RawConfigParser.BOOLEAN_STATES`` to translate strings
        into boolean. See:
        https://github.com/python/cpython/blob/89192c4/Lib/configparser.py#L597-L599
        """
        value = MPMPlugin.getenv_str(var)
        if value is None:
            return default
        return RawConfigParser.BOOLEAN_STATES[value]

    @staticmethod
    def normalize_params(
        font_string: str,
        valid_ids: set[str] | None = None,
    ) -> str:
        if valid_ids is None:
            valid_ids = {"color", "font", "size"}
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
            self.getenv_str("VAR_DEFAULT_FONT", ""),  # type: ignore
        )

    @cached_property
    def monospace_font(self) -> str:
        """Make it easier to change font, sizes and colors of the output."""
        return self.normalize_params(
            self.getenv_str("VAR_MONOSPACE_FONT", "font=Menlo size=12"),  # type: ignore
        )

    @cached_property
    def error_font(self) -> str:
        """Error font is based on monospace font."""
        return self.normalize_params(f"{self.monospace_font} color=red")

    @cached_property
    def is_swiftbar(self) -> bool:
        """SwiftBar is kind enough to tell us about its presence."""
        return self.getenv_bool("SWIFTBAR")

    @cached_property
    def python_path(self) -> str:
        """Returns the system's Python binary path.

        This plugin being run from Python, we have the one called by Xbar/SwiftBar to
        fallback to (i.e. ``sys.executable``). But before that, we attempt to locate it
        by respecting the environment variables.
        """
        for bin_name in ("python", "python3", sys.executable):
            py_path = which(bin_name)
            if py_path:
                return py_path
        raise FileNotFoundError("No Python binary found on the system.")

    @cached_property
    def mpm_exec(self) -> tuple[str, ...]:
        """Search for mpm execution alternatives, either direct ``mpm`` call or as an
        executable Python module."""
        # XXX Local debugging and development.
        # return "poetry", "run", "mpm"
        mpm_exec = which("mpm")
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
                # Output a color-less versionjust in case the script is not run in a
                # non-interactive shell, or Click/Click-Extra autodetection fails.
                (*self.mpm_exec, "--no-color", "--version"),
                capture_output=True,
                encoding="utf-8",
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
            # This regular expression is designed to extract the version number, wether
            # it is surrounded by ANSI color escape sequence or not.
            match = re.compile(
                r"""
                .+                      # Any string
                \                       # A space
                version                 # The "version" string
                \                       # A space
                [^\.]*?                 # Any minimal (non-greedy) string without a dot
                (?P<version>[0-9\.]+)   # Version composed of numbers and dots
                [^\.]*?                 # Any minimal (non-greedy) string without a dot
                $                       # End of the string
                """,
                re.VERBOSE | re.MULTILINE,
            ).search(process.stdout)
            if match:
                version_string = match.groupdict()["version"]
                mpm_version = tuple(map(int, version_string.split(".")))
                # Is mpm too old?
                if mpm_version >= self.mpm_min_version:
                    up_to_date = True

        return installed, mpm_version, up_to_date, error

    @staticmethod
    def pp(label: str, *args: str) -> None:
        """Print one menu-line with the Xbar/SwiftBar dialect.

        First argument is the menu-line label, separated by a pipe to all other non-
        empty parameters, themselves separated by a space.
        """
        print(
            label.strip(),
            "|",
            *(line for line in map(methodcaller("strip"), args) if line),
            sep=" ",
        )

    @staticmethod
    def print_error_header() -> None:
        """Generic header for blocking error."""
        MPMPlugin.pp("❗️", "dropdown=false")
        print("---")

    def print_error(self, message: str | Exception, submenu: str = "") -> None:
        """Print a formatted error message line by line.

        A red, fixed-width font is used to preserve traceback and exception layout. For
        compactness, the message is dedented and empty lines are skipped.

        Message is always casted to a string as we allow passing of exception objects
        and have them rendered.
        """
        for line in map(methodcaller("strip"), dedent(str(message)).splitlines()):
            if line:
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
                # XXX This seems broken beyond repair. No amount of workaround works.
                # See:
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
                msg = (
                    f"{plugin.mpm_exec} is too old: "
                    f"{v_to_str(mpm_version)} < {v_to_str(plugin.mpm_min_version)}"
                )
                raise ValueError(msg)
            print(f"{' '.join(plugin.mpm_exec)} v{v_to_str(mpm_version)}")

        else:
            plugin.print_menu()
