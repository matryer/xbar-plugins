#!/usr/bin/env python3

# <xbar.title>Yabai Viewer</xbar.title>
# <xbar.author>Ryan Moore</xbar.author>
# <xbar.author.github>ryan-mooore</xbar.author.github>
# <xbar.image>https://raw.githubusercontent.com/ryan-mooore/xbar-plugins/master/images/yabai-viewer.png</xbar.image>
# <xbar.desc>Visual indicator of spaces and what apps are on them in your menu bar. Most customisation is below. Customisation of symbols/UI elements can be done at the top of the script (xbar > Open plugin folder...) and find the script for this plugin</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.version>v1.0</xbar.version>

#  <xbar.var>boolean(VAR_SHOW_SPACE_NUMBERS=true): Show numbers of spaces with superscipts</xbar.var>
#  <xbar.var>number(VAR_SHORTEN_APP_NAME=4): Shorten the names of applications to x characters. Set to 0 to turn off</xbar.var>
#  <xbar.var>string(VAR_CANNOT_CONNECT_MESSAGE=Cannot connect to yabai): Message to display when yabai is not runnning. Set to empty string to turn off</xbar.var>

#  <xbar.var>string(VAR_FONT_FACE=""): Choose a custom font face for the plugin to use</xbar.var>
#  <xbar.var>string(VAR_FONT_WEIGHT="Normal"): Weight of chosen custom font face</xbar.var>
#  <xbar.var>number(VAR_FONT_SIZE="12"): Font size of plugin UI</xbar.var>

#  <xbar.var>string(VAR_FOCUSED_COLOR="0"): ANSI escape code for color of focused UI elements (see https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797#256-colors)</xbar.var>
#  <xbar.var>string(VAR_UNFOCUSED_COLOR="38;5;243): ANSI escape code for color of unfocused UI elements (see https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797#256-colors)</xbar.var>

#  <xbar.var>string(VAR_YABAI_PATH=/usr/bin/env yabai): Path to yabai binary</xbar.var>
#  <xbar.var>string(VAR_VIM_PATH=/usr/bin/env vim): Path to Vim binary</xbar.var>
#  <xbar.var>string(VAR_SKHD_PATH=/usr/bin/env skhd): Path to skhd binary</xbar.var>
#  <xbar.var>string(VAR_HOMEBREW_PATH=/usr/bin/env brew): Path to Homebrew binary</xbar.var>

from collections import Counter
from json import loads
from subprocess import CalledProcessError, check_output
from argparse import ArgumentParser
from os.path import realpath

VARS = loads(open(f"{__file__}.vars.json").read())

SUITES = [
    "Microsoft",
    "Google",
    "Adobe",
    "Affinity",
]  # script will remove these words from the start of app names, e.g. Microsoft Word -> Word
MULTIPLY_SYMBOL = "×"
PROPERTIES = {
    "space": {
        "borders": {
            "focused": {"left": "[[", "right": "]]"},
            "unfocused": {"left": "[", "right": "]"},
        },
        "separator": "｜",
        "empty": " ",
    },
    "display": {
        "borders": {
            "focused": {"left": "{{", "right": "}}"},
            "unfocused": {"left": "{", "right": "}"},
        },
        "separator": " ",
        "empty": "-",
    },
    "ui": {"separator": "  ", "empty": "-"},
}

parser = ArgumentParser()
parser.add_argument("--brew_command", action="store", required=False)
args = parser.parse_args()


def service_cmd(cmd: str) -> None:
    for service in ["yabai", "skhd"]:
        try:
            stdout = check_output(
                f"{VARS['VAR_HOMEBREW_PATH']} {cmd} {service}".split(" ")
            ).decode("utf-8")
            if stdout:
                print(stdout)
            else:
                print("Could not complete request")
        except CalledProcessError as e:
            print(e)


def query(domain: str, domain_id: int = None) -> dict:
    command = []
    if domain_id:
        command = [
            VARS["VAR_YABAI_PATH"],
            "-m",
            "query",
            f"--{domain}",
            f"--{domain[:-1]}",
            str(domain_id),
        ]
    else:
        command = [VARS["VAR_YABAI_PATH"], "-m", "query", f"--{domain}"]

    return loads(check_output(command).decode("utf-8"))


def query_focused_display() -> int:
    for space in query("spaces"):
        if space["focused"]:
            return space["display"]
    raise Exception("No spaces focused")


def get_window_app(id: int) -> str:
    window = query("windows", domain_id=id)
    app = window["app"]
    for suite in SUITES:
        if app.startswith(suite):
            app = " ".join(app.split(" ")[1:])
    if VARS["VAR_SHORTEN_APP_NAME"]:
        app = app[: VARS["VAR_SHORTEN_APP_NAME"]]

    return focused(app) if window["focused"] else app


def focused(string: str) -> str:
    return (
        f"\u001b[{VARS['VAR_FOCUSED_COLOR']}m"
        + string
        + f"\u001b[{VARS['VAR_UNFOCUSED_COLOR']}m"
    )


def to_superscript(num: int) -> str:
    return "".join(
        [
            [
                "\u2070",
                "\u00B9",
                "\u00B2",
                "\u00B3",
                "\u2074",
                "\u2075",
                "\u2076",
                "\u2077",
                "\u2078",
                "\u2079",
            ][int(char)]
            for char in str(num)
        ]
    )


def build_ui(
    contents: list, properties: dict, is_focused: bool, suffix: str = None
) -> str:
    def border(side: str) -> str:
        def add_suffix():
            return to_superscript(suffix) if side == "right" and suffix else ""

        if "borders" in properties and properties["borders"]:
            return (
                focused(properties["borders"]["focused"][side] + add_suffix())
                if is_focused
                else properties["borders"]["unfocused"][side] + add_suffix()
            )
        else:
            return ""

    return "".join(
        [
            border("left"),
            properties["separator"].join(contents) if contents else properties["empty"],
            border("right"),
        ]
    )


def ui() -> str:
    display_components = []
    for display in query("displays"):
        space_components = []
        for space_id in display["spaces"]:
            space = query("spaces", domain_id=space_id)

            space_components.append(
                (
                    build_ui(
                        contents=[
                            window + MULTIPLY_SYMBOL + str(count)
                            if count > 1
                            else window
                            for window, count in Counter(
                                [
                                    get_window_app(int(window))
                                    for window in space["windows"]
                                ]
                            ).items()
                        ],
                        properties=PROPERTIES["space"],
                        is_focused=space["focused"],
                        suffix=space["index"]
                        if VARS["VAR_SHOW_SPACE_NUMBERS"]
                        else None,
                    )
                )
            )
        display_components.append(
            build_ui(
                contents=space_components,
                properties=PROPERTIES["display"],
                is_focused=query_focused_display() == display["id"],
            )
        )
    return build_ui(
        contents=display_components, properties=PROPERTIES["ui"], is_focused=False
    )


def main() -> None:
    cannot_connect = False
    if args.brew_command:
        service_cmd(args.brew_command)
    else:
        try:
            print(
                f"\u001b[{VARS['VAR_UNFOCUSED_COLOR']}m"
                + ui()
                + f" | size={VARS['VAR_FONT_SIZE']}"
                + (
                    f" font={VARS['VAR_FONT_FACE'].replace(' ', '')}-{VARS['VAR_FONT_WEIGHT'].replace(' ', '')}"
                    if VARS["VAR_FONT_FACE"]
                    else ""
                )
            )
        except CalledProcessError:
            cannot_connect = True
            print(VARS["VAR_CANNOT_CONNECT_MESSAGE"])

    print("---")
    if cannot_connect:
        print("\u001b[1;31mCannot connect to yabai")
        print(
            "Check your path settings for this plugin in the plugin viewer | color=#ff0000"
        )
    for name, version in {
        "yabai-viewer": "1.0.0",
        "skhd": check_output([VARS["VAR_SKHD_PATH"], "-v"])
        .decode("utf-8")
        .split(" ")[2][:-1],
        "yabai": check_output([VARS["VAR_YABAI_PATH"], "-v"])
        .decode("utf-8")
        .split("-")[1][1:-1],
    }.items():
        print(f"{name} version: {version} | disabled=true | size=10")

    print("Brew services")
    for command in ["start", "stop", "restart"]:
        print(
            f'--{command.title()} yabai & skhd | bash="{realpath(__file__)}" param1=--brew_command param2={command} refresh=true'
        )
    for dotfile in ["yabairc", "skhdrc"]:
        print(
            f'Open {dotfile} | bash={VARS["VAR_VIM_PATH"]} param1="~/.{dotfile}" refresh=true terminal=true'
        )


main()
