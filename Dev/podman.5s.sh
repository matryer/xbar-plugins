#!/usr/bin/env zsh

# Metadata allows your plugin to show up in the app, and website.
#
#  <xbar.title>Podman Manager</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Séamus Ó Ceanainn</xbar.author>
#  <xbar.author.github>soceanainn</xbar.author.github>
#  <xbar.desc>Manage podman using your menu bar</xbar.desc>
#  <xbar.image>https://doc-08-50-docs.googleusercontent.com/docs/securesc/ha0ro937gcuc7l7deffksulhg5h7mbp1/eqk54kg1pese0gadqn723c9dua58epen/1674679425000/15514371234996328630/*/1aV6rbMUHI168ACOr70g9PYSs34BS2iAG?e=view&uuid=60bc4962-8efb-47c6-86d1-9d8eeb4a3710.png</xbar.image>
#  <xbar.dependencies>zsh,podman</xbar.dependencies>

# Variables become preferences in the app:
#
#  <xbar.var>string(VAR_PATH="/usr/local/bin/podman"): Absolute path to podman binary</xbar.var>
#  <xbar.var>string(VAR_BREW_PATH="/usr/local/bin/brew"): Absolute path to Homebrew 'brew' binary</xbar.var>

PURPLE='\033[0;35m'
NC='\033[0m'

# For some reason, this isn't working to open plugin configuration screen (below)
PLUGIN_PATH=`awk -F '/' '{print $NF}'<<<"$0"`

if [[ -f "${VAR_PATH}" ]]; then
  alias podman="${VAR_PATH}"
  running_machine="$(podman machine list | grep 'Currently running' | awk '{print $1}' | sed 's/*//g')"
  if [[ "${running_machine}" != "" ]]; then
    echo -e "${PURPLE}pod${NC}man✅"
    echo "---"
    echo "Currently running: ${running_machine}"
    echo "Stop machine: ${running_machine} | shell=${VAR_PATH} | param1=machine | param2=stop | param3=${running_machine} | refresh=true"
  else
    echo -e "${PURPLE}pod${NC}man❎"
    echo "---"
    echo "Start available machines:"
    all_machines=`podman machine list --noheading --format "{{.Name}}"`
    for machine in `echo ${all_machines}`; do
      echo "-- ${machine} | shell=${VAR_PATH} | param1=machine | param2=start | param3=${machine//\*} | refresh=true"
    done
  fi
else 
  echo -e "${PURPLE}pod${NC}man⚠️"
  echo "---"
  echo "Cannot find Podman at: '${VAR_PATH}'"
  echo "Try installing Podman or updating plugin configuration with corect path"
  if [[ -f ${VAR_BREW_PATH} ]]; then
    echo "Install podman | shell=${VAR_BREW_PATH} | param1=install | param2=podman"
  fi
#  echo "Configure plugin | href=xbar://app.xbarapp.com/openPlugin?path=${PLUGIN_PATH}"
  echo ""
  exit 0
fi

# echo "Configure plugin | href=xbar://app.xbarapp.com/openPlugin?path=${PLUGIN_PATH}"
echo "`podman --version` | disabled=true | size=10"
if [[ -f ${VAR_BREW_PATH} &&`${VAR_BREW_PATH} outdated | grep "podman"` ]]; then
  echo "A new version of podman is available | size 10"
  echo "Upgrade | shell=${VAR_BREW_PATH} | param1=upgrade | param2=podman"
fi
