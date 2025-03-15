#!/usr/bin/env -S PATH="${PATH}:/opt/homebrew/bin:/usr/local/bin" PYTHONIOENCODING=UTF-8 python3

# <xbar.title>LastPass</xbar.title>
# <xbar.version>v1.0.2</xbar.version>
# <xbar.author>Jason Rauen</xbar.author>
# <xbar.author.github>badarsebard</xbar.author.github>
# <xbar.desc>Display your LastPass vault in the menubar. Utilizes the LastPass CLI tool (https://github.com/lastpass/lastpass-cli).</xbar.desc>
# <xbar.image>https://i.imgur.com/zwTCrNO.png</xbar.image>
# <xbar.dependencies>python3,lastpass-cli</xbar.dependencies>
# <xbar.abouturl>https://github.com/badarsebard/xbar-plugins/blob/main/System/lastpass.py</xbar.abouturl>
# <xbar.var>string(VAR_LASTPASS_EMAIL=""): Email address to login to LastPass</xbar.var>
# <xbar.var>select(VAR_TERMINAL="Terminal"): Which terminal app to use. This is used during the login process. [Terminal, iTerm2]</xbar.var>
# <xbar.var>string(VAR_TOP_LEVEL_GROUPS=""): (Optional) Comma separated list of groups/folders to display as the top-level of the menu. Use a slash (/) for sub-groups. Example: Work/SSH.</xbar.var>
# <xbar.var>string(VAR_LASTPASS_CLI_BIN=""): (Optional) Path to the LastPass CLI binary. Defaults to /usr/local/bin/lpass.</xbar.var>
# <xbar.var>string(VAR_FAV_IDS=""): (Optional) Comma separated list of IDs that correspond to your favorites.</xbar.var>

import json
import os
import re
import shlex
import subprocess
import sys

lpass = "/usr/local/bin/lpass"

class App:
    def __init__(self):
        self.email = os.getenv("VAR_LASTPASS_EMAIL", None)
        self.terminal_choice = os.getenv("VAR_TERMINAL", "Terminal")
        self.top_level_groups = os.getenv("VAR_TOP_LEVEL_GROUPS", "").split(",")
        self.favs = os.getenv("VAR_FAV_IDS", "").split(",")
        self.vault = []
        if lpass_cli := os.getenv("VAR_LASTPASS_CLI_BIN"):
            global lpass
            lpass = lpass_cli

    def _logged_in(self):
        status_proc = subprocess.run([lpass, "status"], capture_output=True)
        status_message = status_proc.stdout.decode('utf-8')
        if status_message.startswith("Logged in"):
            return True
        else:
            return False

    def _load_vault(self):
        if not self._logged_in():
            return
        ls = subprocess.run([lpass, "ls"], capture_output=True)
        id_pat = r"\[id\: (\d+)\]"
        cred_ids = re.findall(id_pat, ls.stdout.decode())
        show_args = [lpass, "show", "--all", "--json", *cred_ids]
        show = subprocess.run(show_args, capture_output=True)
        try:
            creds = json.loads(show.stdout.decode())
        except json.JSONDecodeError as e:
            with open("lastpass.py.error.json", "a") as f:
                error = {
                    "exception_type": str(type(e)),
                    "exception": str(e),
                    "raw": show.stdout.decode(),
                }
                f.write(json.dumps(error, indent=2)+"\n")
            return
        for cred in creds:
            if cred["url"] == "http://group":
                continue
            cred["sort"] = cred["name"].lower() \
                if cred["name"] \
                else (cred["url"]
                      .lower()
                      .replace("https://", "")
                      .replace("http://", "")
                      .replace("www.", "")) \
                if cred["url"] \
                else cred["id"]
            cred["display_name"] = cred["name"] \
                if cred["name"] \
                else (cred["url"]
                      .lower()
                      .replace("https://", "")
                      .replace("http://", "")
                      .replace("www.", "")) \
                if cred["url"] \
                else f"id: {cred['id']}"
            if cred['id'] in self.favs:
                cred['fav'] = True
            self.vault.append(cred)
        self.vault.sort(key=lambda x: (x['fullname'], x['sort']))

    def render_favorites(self):
        rendering = []
        favorites = []
        fav_creds = [c for c in self.vault if c.get("fav")]
        for cred in fav_creds:
            id_ = cred["id"]
            display = cred["display_name"]
            favorites.append(f"""{display} | bash={lpass} param1=show param2=--clip param3=--password param4={id_}""")
        if len(favorites) > 0:
            rendering.append("---")
            rendering.append("Favorites")
            rendering.extend(favorites)
        return rendering

    def render_login(self):
        rendering = []
        if not self._logged_in():
            if self.master_password:
                my_env = os.environ.copy()
                my_env["LPASS_DISABLE_PINENTRY"] = "1"
                subprocess.run([lpass, "login", self.email], input=self.master_password, env=my_env, text=True, capture_output=True)
                rendering.append(f"Logout | bash={lpass} param1=logout param2=--force refresh=true")
            else:
                rendering.append(f"Login | bash={sys.argv[0]} param1=login param2={self.email} param3={self.terminal_choice} refresh=true")
        else:
            rendering.append(f"Logout | bash={lpass} param1=logout param2=--force refresh=true")
        rendering.append("---")
        return rendering

    def render_settings_menu(self):
        settings = [
            "---",
            "Settings",
            f"Terminal App",
        ]
        if self.terminal_choice == "Terminal":
            settings.append("--:heavy_check_mark:\tTerminal | href=.")
            settings.append(f"--⠀\tiTerm2 | bash={sys.argv[0]} param1=update_setting param2=VAR_TERMINAL param3=iTerm2 terminal=false refresh=true")
        else:
            settings.append(f"--⠀\tTerminal | bash={sys.argv[0]} param1=update_setting param2=VAR_TERMINAL param3=Terminal terminal=false refresh=true")
            settings.append("--:heavy_check_mark:\tiTerm2 | href=.")
        return settings

    def render_vault(self):
        rendering = []
        credentials = []
        for cred in self.vault:
            grouping = cred["fullname"].split("/")
            group_prefix = ""
            for group in grouping[:-1]:
                group_prefix += "--"
                if group_prefix+group not in credentials:
                    credentials.append(group_prefix+group)
            id_ = cred["id"]
            name = cred["display_name"]
            group_prefix += "--"
            menu_item = f"{group_prefix}{name} | bash={lpass} param1=show param2=--clip param3=--password param4={id_}"
            credentials.append(menu_item)
        top_level_credentials = []
        for top_group in self.top_level_groups:
            last = top_group.split("/")[-1]
            if last not in top_level_credentials:
                top_level_credentials.append(last)
                top_level_credentials.append(f"--{top_group}")
            top_group_creds = [c for c in self.vault if c["fullname"].startswith(top_group)]
            for cred in top_group_creds:
                grouping = cred["fullname"].replace(top_group, "")
                grouping = grouping.lstrip("/")
                grouping = grouping.split("/") if grouping else []
                group_prefix = ""
                for group in grouping[:-1]:
                    group_prefix += "--"
                    if group_prefix+group not in top_level_credentials:
                        top_level_credentials.append(group_prefix+group)
                id_ = cred["id"]
                name = cred["display_name"]
                group_prefix += "--"
                menu_item = f"{group_prefix}{name} | bash={lpass} param1=show param2=--clip param3=--password param4={id_}"
                top_level_credentials.append(menu_item)

        if len(credentials) > 0:
            rendering.append("---")
            rendering.append("Vault")
            rendering.append(self.email)
            rendering.extend(credentials)
            rendering.extend(top_level_credentials)
        return rendering

    def render(self):
        if not self.email:
            print("LastPass | color=red")
            print("---")
            print("Please set the \"LastPass email\" VAR_LASTPASS_EMAIL environment variable")
            return
        rendering = []
        self._load_vault()
        rendering.append("LastPass | refresh=true")
        rendering.append("---")
        rendering.append("Open vault in lastpass.com ⇱| href=https://lastpass.com/vault/")
        if self._logged_in():
            rendering.extend(self.render_favorites())
            rendering.extend(self.render_vault())
        rendering.extend(self.render_settings_menu())
        rendering.extend(self.render_login())
        print("\n".join(rendering))


def main():
    app = App()
    app.render()

if __name__ == '__main__':
    if len(sys.argv) == 1:
        main()
    elif sys.argv[1] == "update_setting":
        with open("lastpass.py.vars.json", "r+") as f:
            settings = json.load(f)
        settings[sys.argv[2]] = sys.argv[3]
        with open("lastpass.py.vars.json", "w") as f:
            json.dump(settings, f)
        subprocess.run(shlex.split("open -jg 'xbar://app.xbarapp.com/refreshAllPlugins'"))
    elif sys.argv[1] == "login":
        email = sys.argv[2]
        terminal_script_with_exit = f"""{lpass} login {email} && open -jg 'xbar://app.xbarapp.com/refreshAllPlugins' && exit"""
        terminal_osascript = f"""
            if application "Terminal" is running then
                tell application "Terminal"
                    if not (exists window 1) then
                        reopen
                        activate
                        do script "{terminal_script_with_exit}" in window 1
                    else
                        reopen
                        activate
                        do script "{lpass} login {email}" in window 1
                    end if
                end tell
            else
                tell application "Terminal"
                    activate
                    do script "{terminal_script_with_exit}" in window 1
                end tell
            end if
        """
        iterm2_osascript = f"""
            tell application "iTerm"
                set newWindow to (create window with default profile)
                tell current session of newWindow
                    write text "{terminal_script_with_exit}"
                end tell
            end tell
        """
        terminal_cmds = {
                "Terminal": f"""osascript -e '{terminal_osascript}'""",
                "iTerm2": f"""osascript -e '{iterm2_osascript}'""",
        }
        subprocess.run(shlex.split(terminal_cmds[sys.argv[3]]))
