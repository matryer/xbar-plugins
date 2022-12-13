#!/bin/bash
#
# <xbar.title>Real CPU Usage Chart (▁▃▃█▇▃)</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Marian Schubert</xbar.author>
# <xbar.author.github>maio</xbar.author.github>
# <xbar.desc>Chart CPU usage over last minute.</xbar.desc>
# <xbar.image>https://cloud.githubusercontent.com/assets/18138/12124861/27f42162-b3e8-11e5-845e-38bec5433d03.png</xbar.image>
# <xbar.dependencies>spark</xbar.dependencies>
#
# Based on work by Mat Ryer and Tyler Bunnell
#
# This script requires https://github.com/holman/spark

SPARK="/usr/local/bin/spark"

if [ ! -f "${SPARK}" ]; then
    echo "Install spark utility please."
    exit 1
fi

IDLE=$(top -F -R -l3 | grep "CPU usage" | tail -1 | \
      egrep -o '[0-9]{0,3}\.[0-9]{0,2}% idle' | sed 's/% idle//')

CURRENT=$(printf "%.0f" "$(echo 100 - "$IDLE" | bc)")

# Let's put/keep last 6 values (= one minute) in HISTORY_FILE
HISTORY_FILE="${HOME}/.cpu.history"
touch "${HISTORY_FILE}"
PREVIOUS=$(tail -5 "${HISTORY_FILE}")
echo "$PREVIOUS" > "${HISTORY_FILE}"
echo "$CURRENT" >> "${HISTORY_FILE}"

# Spark outputs unicode characters so let's make sure we can handle
# them correctly.
LC_ALL=en_US.UTF-8

# Spark uses maximum input value to define scale so let's make sure
# that input contains at least one 100(%). Strip it afterwards.
CHART=$( ( echo 100 ; cat "${HISTORY_FILE}" ) | ${SPARK})
echo "${CHART:1}"
