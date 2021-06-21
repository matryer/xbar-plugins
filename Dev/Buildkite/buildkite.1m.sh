#!/usr/bin/env bash

# List recent Buildkite builds
#
# Example output:
#  $ ./bitbar.sh
#  🚀:1
#  ---
#  ✅ Pipeline 1: master [nick@app…]
#  ⏸ Pipeline 2: master [nick@app…]
#  ⏹ Pipeline 2: flaky-test [foo@app…]

# <bitbar.title>Buildkite Recent Builds</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Nicholas Edwards</bitbar.author>
# <bitbar.author.github>edwardsnjd</bitbar.author.github>
# <bitbar.desc>List all recent builds you can see in Buildkite.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/edwardsnjd/bitbar-buildkite/master/screenshot.gif</bitbar.image>
# <bitbar.dependencies>bash,curl,jq,buildkite.com</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/edwardsnjd/bitbar-buildkite/</bitbar.abouturl>

# Bash sanity:

set -o errexit   # abort on nonzero exitstatus
set -o errtrace  # abort on error inside any functions or subshells
set -o nounset   # abort on unbound variable
set -o pipefail  # don't hide errors within pipes
# set -o xtrace  # turn on traces, useful while debugging

# Prerequisites:

# Supply your API Token from Buildkite
BUILD_KITE_API_TOKEN="ABC" # <- replace ABC with your token

# Adjust these paths if necessary for your system
CURL="curl"
JQ="/usr/local/bin/jq"

# Configuration:

HOURS_BACK_TO_FETCH=6
ICONS_JSON='{
  "main": "🚀",
  "running": "▶️",
  "passed": "✅",
  "failed": "❌",
  "canceled": "⏹",
  "paused": "⏸"
}'

# Functions:

# -- HTTP with curl --

# fetch_recent_builds_json :: () => Json[]
function fetch_recent_builds_json() {
  local -r since=$(date -u -v-${HOURS_BACK_TO_FETCH}H +"%Y-%m-%dT%H:%MZ")
  local -r url="https://api.buildkite.com/v2/builds?created_from=${since}"
  local -r auth_header="Authorization: Bearer ${BUILD_KITE_API_TOKEN}"

  "${CURL}" --silent -H "${auth_header}" "${url}"
}

# -- JSON transformation with jq --

# run_jq :: Any* -> ?
function run_jq() {
  # Supply common config to all invocations
  "${JQ}" \
    --argjson icons "${ICONS_JSON}" \
    --argjson hours_back "${HOURS_BACK_TO_FETCH}" \
    "$@"
}

# transform_builds :: Json[] -> Json[]
function transform_builds() {
  run_jq -r 'sort_by(.pipeline.name)'
}

# format_active_builds :: Json[] -> String
function format_active_builds() {
  # shellcheck disable=SC2016
  run_jq -r '
    # is_active_job :: Job -> bool
    def is_active_job:
      .state == "running";

    # suffix :: Job[] -> String
    def suffix:
      (map(select(is_active_job)) | length) as $total_active
      | if $total_active > 0 then ":\( $total_active )" else "" end;

    # icon :: String -> String
    def icon(name):
      $icons[name] // name;

    "\( icon("main") ) \( suffix )"
 '
}

# format_builds :: Json[] -> String[]
function format_builds() {
  # shellcheck disable=SC2016
  run_jq -r '
    # is_real_job :: Job -> bool
    def is_real_job:
      .type != "waiter";

    # is_finished_job :: Job -> bool
    def is_finished_job:
      [.state]
      | inside(["passed", "failed", "broken", "timed_out", "canceled" ]);

    # str_trunc(after) :: String -> String
    def str_trunc(after):
      if length > after then "\( .[0:after] )…"
      else . end;

    # icon :: String -> String
    def icon(name):
      $icons[name] // name;

    # build_state_icon :: Job -> String
    def build_state_icon:
      icon(.state);

    # build_job_count :: Job -> String
    def build_job_count:
      .jobs
      | map(select(is_real_job))
      | length as $total
      | ( map(select(is_finished_job)) | length ) as $finished
      | "\($finished)/\($total)";

    # build_creators_by_commit :: Job -> { String: String, ... }
    def build_creators_by_commit:
      map({ key: .commit, value: .creator.email })
      | sort_by(.value)
      | from_entries;

    # color_directive :: Job -> String
    def color_directive:
      if (.state == "running") then "color=white"
      else "" end;

    build_creators_by_commit as $build_creators
    | .[]
    # main row
    | "\( build_state_icon )"
      + " \( .pipeline.name | str_trunc(15) ):"
      + " \( .branch | str_trunc(25) )"
      + " [\( $build_creators[.commit] // "?" | str_trunc(8) )]"
      + "|\( color_directive )"
    # alternate row (shown with Option key)
    , "\( build_state_icon )"
      + " \( .pipeline.name | str_trunc(15) ):"
      + " \( .commit[0:8] )"
      + " [\( build_job_count )]"
      + "|\( color_directive ) alternate=true"
  '
}

# format_blank_slate :: Json[] -> String[]
function format_blank_slate() {
  # shellcheck disable=SC2016
  run_jq -r '
    if length == 0 then
      "No recent builds",
      "No recent builds (in last \( $hours_back ) hours)|alternate=true"
    else "" end
  '
}

# Output lines

# print_bar_row :: () -> String
function print_bar_row() {
  echo "${builds}" | format_active_builds
}

# print_popup_rows :: () -> String[]
function print_popup_rows() {
  echo "${builds}" | format_builds
}

# print_popup_blank_slate :: () -> String[]
function print_popup_blank_slate() {
  echo "${builds}" | format_blank_slate
}

# print_separator :: () -> String
function print_separator() {
  echo "---"
}

# print_instructions :: () -> String[]
function print_instructions() {
  echo "Hold Option key for alternate view"
  echo "Release Option key for default view|alternate=true"
}

# Orchestration:

# main :: () -> String[]
function main() {
  # Get the data
  local -r builds=$(fetch_recent_builds_json | transform_builds)

  # Print bitbar output
  print_bar_row "${builds}"
  print_separator
  print_popup_rows "${builds}"
  print_popup_blank_slate "${builds}"
  print_separator
  print_instructions
}

# Do it!
main
