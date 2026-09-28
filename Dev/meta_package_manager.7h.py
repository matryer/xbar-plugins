#!/usr/bin/env python3
# <xbar.title>Meta Package Manager</xbar.title>
# <xbar.version>8.0.2</xbar.version>
# <xbar.author>Kevin Deldycke</xbar.author>
# <xbar.author.github>kdeldycke</xbar.author.github>
# <xbar.desc>List outdated packages and manage upgrades.</xbar.desc>
# <xbar.dependencies>python,mpm</xbar.dependencies>
# <xbar.image>https://raw.githubusercontent.com/kdeldycke/meta-package-manager/refs/heads/main/docs/assets/xbar-submenu-table-rendering.png</xbar.image>
# <xbar.abouturl>https://mpm.run/bar-plugin/</xbar.abouturl>
# XXX SwiftBar requires quotes around a default value. Xbar accepts them and removes
# them. Without the quotes, SwiftBar ignores the variable and never shows it in its
# settings UI.
# <xbar.var>boolean(VAR_GROUP_BY_MANAGER="true"): Group each manager's packages into a section of its own.</xbar.var>
# <xbar.var>boolean(VAR_ALIGN_COLUMNS="true"): Centers versions around the arrow and aligns names in a monospaced font.</xbar.var>
# <xbar.var>number(VAR_MAX_VERSION_WIDTH="18"): Widest a version renders in a menu line, in characters. Longer ones are shortened with an ellipsis.</xbar.var>
# <xbar.var>string(VAR_MPM_OPTIONS=""): Extra options for every mpm call the plugin makes, placed before the subcommand.</xbar.var>
# XXX The font options are for SwiftBar only: Xbar cuts a default value at its first
# `=` character. See: https://github.com/matryer/xbar/issues/832
# <swiftbar.var>string(VAR_DEFAULT_FONT=""): Font parameters for regular text.</swiftbar.var>
# <swiftbar.var>string(VAR_MONOSPACE_FONT="font=Menlo size=12"): Font parameters for monospace text. Used for table rendering and error messages.</swiftbar.var>
# XXX SwiftBar alone hides a plugin that produces no output, so this variable is for
# SwiftBar only too.
# <swiftbar.var>boolean(VAR_ALWAYS_VISIBLE="true"): Keep the menu bar icon while no package is outdated and no manager reports an error.</swiftbar.var>
"""SwiftBar and Xbar plugin for Meta Package Manager (the {command}`mpm` CLI).

Set the update cycle to several hours. One notice a day is enough, and each
check uses many resources. A short cycle can also use up the GitHub API quota
that Homebrew needs.

Both hosts give the plugin options to the script as environment variables:

- [Xbar passes them](https://xbarapp.com/docs/2021/03/14/variables-in-xbar.html)
  from its settings UI to the script.

- SwiftBar does the same since `2.1.0` added support for `xbar.var`
  ([swiftbar/SwiftBar#472](https://github.com/swiftbar/SwiftBar/pull/472)).
"""

from __future__ import annotations

import argparse
import os
import re
import sys
from configparser import RawConfigParser
from functools import cached_property
from operator import attrgetter, methodcaller
from pathlib import Path
from shlex import shlex
from shutil import which
from subprocess import run
from textwrap import dedent
from typing import NamedTuple

TYPE_CHECKING = False
if TYPE_CHECKING:
    from collections.abc import Generator


SWIFTBAR_MIN_VERSION = (2, 1, 0)
"""SwiftBar `2.1.0` fixes a bug with multiple parameters in a font string.

The fix first went out in a test build labelled `2.1.2`. No release used that
number: the public releases numbered it `2.1.0`. A floor at `2.1.2` would
exclude every SwiftBar release.

See [swiftbar/SwiftBar#445](https://github.com/swiftbar/SwiftBar/issues/445).
"""

MPM_MIN_VERSION = (5, 0, 0)
"""`mpm` `5.0.0` is the first version that renders the complete menu layout."""

SHELL_ENV_MIN_VERSION = (8, 0, 0)
"""First `mpm` release accepting `--shell-env`, which the plugin passes to it.

Both hosts start this plugin outside a terminal. SwiftBar runs it through
`$SHELL -l -c`, a login shell that reads `.zprofile` and skips `.zshrc`, and Xbar
runs it with no shell at all, on the `PATH` of the desktop session. The option
makes `mpm` adopt the environment of an interactive login shell before it looks
for managers, so the menu lists what a terminal would. An older `mpm` gets no
such option: it would reject it as unknown, which is worse than the narrow
`PATH`.

The GNOME Shell extension carries the same gate, and
`tests/test_gnome_extension.py` holds the two in sync.
"""

INSTALL_ARGV = ("uv", "tool", "install", "--upgrade", "meta-package-manager")
"""Install command offered when no runnable `mpm` is found.

A global [`uv tool`](https://docs.astral.sh/uv/concepts/tools/) install is the
first method on the [installation page](https://mpm.run/install/). It puts
`mpm` on the `PATH` of every shell. A `pip install` puts it in one
interpreter, which may not be the one that runs this plugin. `--upgrade` also
covers an `mpm` that is too old, so the command needs no version specifier:
the latest release always satisfies {data}`MPM_MIN_VERSION`. Xbar also breaks
a quoted `>=` specifier
([matryer/xbar#831](https://github.com/matryer/xbar/issues/831)).

The GNOME Shell extension offers the same command in its own menu for a
missing `mpm`. `tests/test_gnome_extension.py` checks that the two are equal.
"""

INSTALL_DOCS_URL = "https://mpm.run/install/"
"""Installation page, offered beside {data}`INSTALL_ARGV`.

`uv` may be missing too, and it is not the only method: a distribution
package, Homebrew or a standalone binary also install `mpm`.
"""

PLUGIN_DOCS_URL = "https://mpm.run/bar-plugin/"
"""Documentation of this plugin, linked from the About submenu.

This is the same address as the one in the `<xbar.abouturl>` header. The host
shows that header in its plugin browser only, and never in the menu.
"""

MPM_TIMEOUT = 60
"""Maximum duration in seconds for one `mpm` call.

Passed as `--timeout` to every `mpm` call. The `mpm` defaults are set for
interactive use and are too long for a background menu bar refresh: 120
seconds for a read-only query, and 500 seconds for an operation that changes
state, like `sync`. With this limit, a hung package manager fails the refresh
after one minute, and it does not block the menu bar for several minutes.
"""

VERSION_REGEX = re.compile(
    r"""
    .+                      # Any string
    \                       # A space
    version                 # The "version" string
    \                       # A space
    [^\.]*?                 # Any minimal (non-greedy) string without a dot
    (?P<release>
      (?P<version>[0-9]+(?:\.[0-9]+)+) # Version composed of numbers and dots
      [^\s\x1b]*            # Any suffix, stopping short of an ANSI escape
    )
    .*?                     # Any trailing string (ANSI codes, etc.)
    $                       # End of the string
    """,
    re.VERBOSE | re.MULTILINE,
)
"""Both readings of the version that `mpm --version` prints, with or without
ANSI colors.

`version` is the numeric part, and `release` is the whole token. A development
build reports `8.0.0.dev0+40ce0879`, and its suffix identifies the build.
"""


class Candidate(NamedTuple):
    """One way to run `mpm` found on the system, and the result of its version
    probe.

    `version` is the tuple that the code compares with {data}`MPM_MIN_VERSION`,
    and `release` is the token as printed. The `probeMpm()` function of the
    GNOME Shell extension returns the same fields.
    """

    args: tuple[str, ...]
    runnable: bool = False
    up_to_date: bool = False
    version: tuple[int, ...] | None = None
    error: str | Exception | None = None
    release: str | None = None

    @property
    def rank(self) -> tuple[bool, bool, tuple[int, ...]]:
        """Sort key of {attr}`MPMPlugin.ranked_mpm`: runnable first, then up to
        date, then the highest version.

        `error` and `release` are not part of the key. Python cannot compare two
        exceptions, and it cannot compare a missing version with a found one.
        Either case would make the sort raise an error.
        """
        return self.runnable, self.up_to_date, self.version or ()


class MPMPlugin:
    """Finds the `mpm` CLI on the system and calls it.

    `mpm` produces the main output of the plugin.

    The output must follow both the
    [Xbar dialect](https://github.com/matryer/xbar-plugins/blob/main/CONTRIBUTING.md#plugin-api)
    and the
    [SwiftBar dialect](https://github.com/swiftbar/SwiftBar#plugin-api).
    """

    @staticmethod
    def getenv_str(var: str, default: str | None = None) -> str | None:
        """Read an environment variable.

        All environment variables are strings. This method always returns a
        lowercase string.
        """
        value = os.environ.get(var, None)
        if value is None:
            return default
        return str(value).lower()

    @staticmethod
    def getenv_bool(var: str, default: bool = False) -> bool:
        """Read an environment variable as a boolean.

        Uses
        [`configparser.RawConfigParser.BOOLEAN_STATES`](https://github.com/python/cpython/blob/3c298e2e385fc6f462abaada2fd680deb1a2b58e/Lib/configparser.py#L596-L597)
        to convert the string.
        """
        value = MPMPlugin.getenv_str(var)
        if value is None:
            return default
        return RawConfigParser.BOOLEAN_STATES[value]

    @staticmethod
    def getenv_int(var: str, default: int) -> int:
        """Read an environment variable as an integer.

        Returns the default when the value is not a number. A typo in a plugin
        setting then changes the layout, and the menu still works.
        """
        value = MPMPlugin.getenv_str(var)
        if value is None:
            return default
        try:
            return int(value)
        except ValueError:
            return default

    @staticmethod
    def normalize_params(font_string: str, valid_ids: set[str] | None = None) -> str:
        """Parse a string of parameters and return a normalized string.

        The input is a space-separated list of parameters. Each parameter is a
        key/value pair separated by an equal sign.

        The method keeps the parameters named in `valid_ids` and ignores the
        others. By default it keeps `color`, `font` and `size`.

        One parameter can occur more than once. The method keeps the last value.

        Both hosts document the available parameters:

        - [SwiftBar](https://github.com/swiftbar/SwiftBar?tab=readme-ov-file#parameters)
        - [Xbar](https://github.com/matryer/xbar-plugins/blob/main/CONTRIBUTING.md#parameters)
        """
        if not valid_ids:
            valid_ids = {"color", "font", "size"}
        params = {}

        # shlex returns `=` as a separate token, so a value is the token after it.
        key = None
        after_separator = False
        for token in shlex(font_string):
            if token == "=":
                after_separator = True
            elif after_separator:
                if key and key in valid_ids:
                    params[key] = token
                after_separator = False
                key = None
            else:
                key = token

        return " ".join(f"{k}={v}" for k, v in params.items())

    @staticmethod
    def str_to_version(version_string: str | None) -> tuple[int, ...]:
        """Convert a version string into a tuple of integers."""
        if not version_string:
            return ()
        return tuple(map(int, version_string.strip().split(".")))

    @staticmethod
    def version_to_str(version_tuple: tuple[int, ...] | None) -> str:
        """Convert a tuple of integers into a version string."""
        if not version_tuple:
            return "None"
        return ".".join(map(str, version_tuple))

    @cached_property
    def align_columns(self) -> bool:
        """Center each version pair around its arrow, and align the package names.

        The rows use a fixed-width font, so every padding space has the same
        width. See
        {meth}`~meta_package_manager.bar_plugin_renderer.BarPluginRenderer.align_rows`
        for the reason to align on the arrow.
        """
        return self.getenv_bool("VAR_ALIGN_COLUMNS", True)

    @cached_property
    def plugin_version(self) -> str:
        """Version that this script reports to its host.

        Read from the `<xbar.version>` header. That header is the only place
        where the number is written: both hosts parse it from the source, and
        `bump-my-version` updates it at each release. A constant beside it
        would be a second copy, and the two could become different.
        """
        try:
            source = Path(__file__).read_text(encoding="UTF-8")
        except OSError:
            return "unknown"
        match = re.search(r"<xbar\.version>(?P<version>[^<]+)</xbar\.version>", source)
        return match.group("version") if match else "unknown"

    @cached_property
    def mpm_options(self) -> tuple[str, ...]:
        """Options added to every `mpm` call.

        They go after the options of the plugin and before the subcommand. A
        single-value option that occurs twice, like `--verbosity`, then takes
        the value from the user.

        The version probe does not use them. A mistyped option then makes
        `sync` fail with mpm's own usage error, which the menu shows.
        Without this, the same typo would look like a missing `mpm`. The
        upgrade actions of the menu use these options too:
        {class}`~meta_package_manager.bar_plugin_renderer.BarPluginRenderer`
        builds them inside the `outdated` call and inherits this environment.

        The value comes from `VAR_MPM_OPTIONS`, read without {meth}`getenv_str`
        because that method lowercases it. A `--verbosity INFO` level and a
        path in `--config` must keep their case. The GNOME Shell extension's
        `mpm-options` setting is the equivalent option.
        """
        lexer = shlex(os.environ.get("VAR_MPM_OPTIONS", ""), posix=True)
        # Same setting as `shlex.split()`: tokens break on whitespace only, so
        # `--config=/a/b` and `1:2` each stay one argument.
        lexer.whitespace_split = True
        return tuple(lexer)

    @cached_property
    def always_visible(self) -> bool:
        """Keep the menu bar icon when there is nothing to report.

        SwiftBar hides a plugin that produces no output. To make the icon
        disappear, the plugin renders nothing. The plugin must then separate an
        empty output that the user asked for from the empty output of a failed
        `mpm` call. For this reason, {meth}`print_menu` accepts an empty output
        only when this option is set.

        Xbar does not hide a plugin, so the variable is declared for SwiftBar
        only.

        The value comes from the `VAR_ALWAYS_VISIBLE` environment variable. The
        name matches the `always-visible` setting of the GNOME Shell extension.
        """
        return self.getenv_bool("VAR_ALWAYS_VISIBLE", True)

    @cached_property
    def default_font(self) -> str:
        """Font, size and color of the regular text of the menu."""
        return self.normalize_params(
            self.getenv_str("VAR_DEFAULT_FONT", ""),  # type: ignore
        )

    @cached_property
    def monospace_font(self) -> str:
        """Font, size and color of the monospace text of the menu."""
        return self.normalize_params(
            self.getenv_str("VAR_MONOSPACE_FONT", "font=Menlo size=12"),  # type: ignore
        )

    @cached_property
    def error_font(self) -> str:
        """Error font: the monospace font, in red.

        It sets no size of its own. The menu gives a row the height of the
        largest font in it, and both hosts put the extra space under the text
        instead of dividing it between the rows. With `size=10`, an error line
        had 15 pixels of space below it, and every other row had 9.
        """
        return self.normalize_params(f"{self.monospace_font} color=red")

    @cached_property
    def is_swiftbar(self) -> bool:
        """`True` when the host is SwiftBar, which sets the `SWIFTBAR` variable."""
        return self.getenv_bool("SWIFTBAR")

    @staticmethod
    def search_venv(folder: Path) -> tuple[str, ...] | None:
        """Return the command that runs `mpm` from the project in `folder`, or
        `None`.

        A folder is a project when it holds a lockfile. The command is the one
        that the project's tool provides to run a program inside the environment
        it manages. The result never starts with an environment
        assignment: {meth}`check_mpm` runs the command without a shell, and a
        shell-less call would read the assignment as the program name. The
        other candidates of {meth}`search_mpm` cover a folder with no
        lockfile.
        """
        if (folder / "uv.lock").is_file():
            # `--frozen` and `--project` point at the folder that holds the
            # lockfile. A bare `uv run` would use the working directory of the
            # process instead, and would lock that other project again at each
            # start, with the uv configuration of the user included.
            return ("uv", "run", "--frozen", "--project", str(folder), "mpm")

        if (folder / "poetry.lock").is_file():
            return ("poetry", "run", "--directory", str(folder), "mpm")

        return None

    def search_mpm(self) -> Generator[tuple[str, ...], None, None]:
        """Yield each command that can run `mpm`.

        The method covers every context of a system where `mpm` may be
        installed.

        {attr}`ranked_mpm` keeps the order of these candidates.

        The commands from a virtual environment come first. The plugin prefers
        the `mpm` that it belongs to. This file is part of the package, so the
        walk up its parent folders reaches the project that installed it. That
        `mpm` is the one released with this plugin, and its dependencies are
        already resolved. Both hosts put a symlink to this file in their own
        plugin folder, which is why the code below resolves the path first. An
        unresolved path would walk that plugin folder, find nothing, and leave
        the plugin with another `mpm` from the system.

        The other candidates are for a plugin installed in the host alone: a
        system-wide installation, then the module under an interpreter.
        {meth}`check_mpm` runs each candidate before the ranking, so none of
        them is accepted without a test.
        """
        for folder in Path(__file__).resolve().parents:
            # Stop at the home folder. It is not a project of the user, and
            # neither is any folder above it. Without this stop, the walk goes
            # up to `/` and searches every shared parent folder, where an
            # unrelated lockfile can be.
            if folder == Path.home():
                break

            venv_cli = self.search_venv(folder)
            if not venv_cli:
                continue

            yield venv_cli

        mpm_bin = which("mpm")
        if mpm_bin:
            yield (mpm_bin,)

        seen = set()
        for py_path in (sys.executable, which("python3")):
            if not py_path:
                continue
            # The deduplication uses the path as written, not the path it
            # points to. An interpreter in a virtual environment is a symlink
            # to the interpreter it was built from, and that second one cannot
            # see the packages of the environment. Two names for one
            # interpreter cost one extra probe here. A resolved key would
            # remove one environment that holds `mpm`.
            normalized = os.path.normcase(py_path)
            if normalized in seen:
                continue
            seen.add(normalized)
            yield (py_path, "-m", "meta_package_manager")

    def check_mpm(self, mpm_cli_args: tuple[str, ...]) -> Candidate:
        """Run the version probe of one command and read the result.

        `--no-color` keeps the output parseable when Click detects a terminal.
        A command with a missing program is a candidate like the others, and
        its error is the exception.
        """
        try:
            process = run(
                (*mpm_cli_args, "--no-color", "--version"),
                capture_output=True,
                encoding="utf-8",
                check=False,
            )
        except FileNotFoundError as ex:
            return Candidate(mpm_cli_args, error=ex)
        if process.returncode or process.stderr:
            return Candidate(mpm_cli_args, error=process.stderr)

        match = VERSION_REGEX.search(process.stdout)
        if not match:
            return Candidate(mpm_cli_args, runnable=True, error=process.stderr)
        version = self.str_to_version(match.group("version"))
        return Candidate(
            mpm_cli_args,
            runnable=True,
            up_to_date=version >= MPM_MIN_VERSION,
            version=version,
            error=process.stderr,
            release=match.group("release"),
        )

    @cached_property
    def ranked_mpm(self) -> list[Candidate]:
        """Every `mpm` found on the system, best candidate first.

        Sorted on {attr}`Candidate.rank`. Two candidates with the same rank
        keep the order that {meth}`search_mpm` gave them.
        """
        return sorted(
            map(self.check_mpm, self.search_mpm()),
            key=attrgetter("rank"),
            reverse=True,
        )

    @cached_property
    def best_mpm(self) -> Candidate:
        return self.ranked_mpm[0]

    @property
    def shell_env_options(self) -> tuple[str, ...]:
        """`--shell-env` for an `mpm` that accepts it, nothing for an older one.

        Goes right after the `mpm` command and ahead of {attr}`mpm_options`, so
        a `--no-shell-env` typed into `VAR_MPM_OPTIONS` wins.
        """
        version = self.best_mpm.version
        if version and version >= SHELL_ENV_MIN_VERSION:
            return ("--shell-env",)
        return ()

    @staticmethod
    def pp(label: str, *args: str | None) -> None:
        """Print one menu line in the SwiftBar and Xbar dialect.

        The first argument is the label of the menu line. A pipe separates it
        from the other parameters, and a space separates those parameters from
        each other.

        The method prints nothing when the label is empty. A `None` parameter
        renders nothing, so a package with no upgrade command still gets a menu
        line with its label alone.
        """
        if label.strip():
            print(
                # The label is not stripped, to keep the alignment of the
                # characters in a table and in a Python traceback.
                label,
                "|",
                *(arg.strip() for arg in args if arg and arg.strip()),
                sep=" ",
            )

    @staticmethod
    def print_error_header() -> None:
        """Print the menu header of a blocking error."""
        MPMPlugin.pp("❗️", "dropdown=false")
        print("---")

    def print_error(self, message: str | Exception, submenu: str = "") -> None:
        """Print an error message, with one menu line per line of text.

        The lines use a red, fixed-width font, which keeps the layout of a
        traceback or of an exception. The message is dedented and its empty
        lines are removed, to keep the block compact.

        The message is converted to a string, so the caller can pass an
        exception object.
        """
        for line in map(methodcaller("rstrip"), dedent(str(message)).splitlines()):
            if line:
                self.pp(
                    f"{submenu}{line}",
                    self.error_font,
                    "trim=false",
                    "ansi=false",
                    "emojize=false",
                    "symbolize=false" if self.is_swiftbar else "",
                )

    def print_about(self) -> None:
        """Print the menu footer, with the version of the plugin and the
        version of `mpm`.

        This script and `mpm` are installed and updated separately. A plugin
        file copied into the host's folder keeps its version while `mpm`
        changes, and no other part of the menu shows the difference. The `mpm`
        line reports the release as printed, with its suffix. The ranking
        compares the numeric part only.

        The footer is one collapsed row. Three visible rows would push the
        package list down for a user who opened the menu to read it.
        """
        host = "SwiftBar" if self.is_swiftbar else "Xbar"
        best = self.best_mpm
        print("---")
        self.pp("About", self.default_font)
        self.pp(
            f"--Meta Package Manager ({host} plugin) {self.plugin_version}",
            self.default_font,
        )
        self.pp(
            f"--mpm {best.release}" if best.runnable else "--mpm not found",
            self.default_font,
        )
        if best.runnable:
            self.pp(f"--{' '.join(best.args)}", self.monospace_font)
        self.pp("--Documentation", f"href={PLUGIN_DOCS_URL}", self.default_font)

    def print_menu(self) -> None:
        """Print the main menu."""
        # Xbar gives no version to its plugins, so only SwiftBar is checked.
        if self.is_swiftbar:
            swiftbar_version_str = self.getenv_str("SWIFTBAR_VERSION", "")
            swiftbar_version = self.str_to_version(swiftbar_version_str)
            if not swiftbar_version or swiftbar_version < SWIFTBAR_MIN_VERSION:
                self.print_error_header()
                self.print_error(
                    f"SwiftBar v{swiftbar_version_str} found, but "
                    f"v{self.version_to_str(SWIFTBAR_MIN_VERSION)} is required.",
                )
                return

        best = self.best_mpm
        if not best.runnable or not best.up_to_date:
            self.print_error_header()
            if best.error:
                self.print_error(best.error)
                print("---")
            action_msg = "Install" if not best.runnable else "Upgrade"
            min_version_str = self.version_to_str(MPM_MIN_VERSION)
            self.pp(
                f"{action_msg} mpm >= {min_version_str} with uv",
                f"shell={INSTALL_ARGV[0]}",
                *(
                    f"param{index}={arg}"
                    for index, arg in enumerate(INSTALL_ARGV[1:], start=1)
                ),
                self.error_font,
                "refresh=true",
                "terminal=true",
            )
            self.pp(
                "Open mpm installation instructions",
                f"href={INSTALL_DOCS_URL}",
                self.error_font,
            )
            self.print_about()
            return

        # Refresh the index of every manager first. A failure is not fatal.
        # `sync` reports its state through the per-manager trail, which prints
        # on any stream: uncaptured here, it would land in the host app's log
        # on every refresh, so the plugin turns it off rather than read it.
        run(
            (
                *best.args,
                *self.shell_env_options,
                "--verbosity",
                "ERROR",
                "--no-progress",
                "--timeout",
                str(MPM_TIMEOUT),
                *self.mpm_options,
                "sync",
            ),
            check=False,
        )

        # `mpm` renders the menu itself. A manager's errors come back in
        # its own section, so only the CRITICAL logs are shown.
        process = run(
            (
                *best.args,
                *self.shell_env_options,
                "--verbosity",
                "CRITICAL",
                "--timeout",
                str(MPM_TIMEOUT),
                *self.mpm_options,
                "outdated",
                "--plugin-output",
            ),
            capture_output=True,
            encoding="utf-8",
            check=False,
        )

        # Stop on an error in the execution of `mpm`, or when `mpm` produces no
        # output. An empty output is expected when the user asked to hide the
        # icon while every package is up to date. The plugin then sends that
        # empty output to the host, which hides the plugin.
        # The exit code is the test for a failed run. `<stderr>` alone is not
        # enough: a `--verbosity` option from `VAR_MPM_OPTIONS` replaces the
        # CRITICAL value above, so a successful run writes to `<stderr>` too.
        if process.returncode or (not process.stdout and self.always_visible):
            self.print_error_header()
            self.print_error(process.stderr)
            self.print_about()
            return

        # print() adds a line break, and the captured output already has one.
        if process.stdout:
            print(process.stdout.rstrip())
            self.print_about()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--search-mpm",
        action="store_true",
        help="Find every mpm on the system and sort them, best candidate first.",
    )
    args = parser.parse_args()

    plugin = MPMPlugin()

    if args.search_mpm:
        for candidate in plugin.ranked_mpm:
            print(
                f"{' '.join(candidate.args)} | runnable: {candidate.runnable} | "
                f"up to date: {candidate.up_to_date} | version: {candidate.version} | "
                f"release: {candidate.release} | error: {candidate.error!r}"
            )

    else:
        plugin.print_menu()
