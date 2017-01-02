#!/usr/bin/env bash
#
# <bitbar.title>SSH Status</bitbar.title>
# <bitbar.version>v0.1.0</bitbar.version>
# <bitbar.author>Olivier Tille</bitbar.author>
# <bitbar.author.github>oliviernt</bitbar.author.github>
# <bitbar.image>http://i.imgur.com/s8FMCLG.png</bitbar.image>
# <bitbar.desc>Continuously checks if hosts are available for ssh connection</bitbar.desc>
# <bitbar.dependencies>Bash</bitbar.dependencies>
#
# SSH Status plugin
# by Olivier Tille (@oliviernt)
#
# Continuously checks if hosts are available for ssh connection on port 22
HOSTS=("example1.com" "example2.com")

head="ssh status"
body=""
errors=0
for host in "${HOSTS[@]}"; do
  if nc -z -G 2 "$host" 22 &> /dev/null; then
    body="$body\n☀️ $host is up | color=green bash=ssh param1=$host"
  else
    body="$body\n⛈ $host looks down from here | color=red bash=ssh param1=$host"
    errors=$((errors + 1))
  fi
  body="$body\n---"
done

echo -n "$head"
[ "$errors" -eq 0 ] || echo -n " ($((${#HOSTS[@]} - errors))/${#HOSTS[@]}) | color=red"
echo -e "\n---"
echo -e "$body"
