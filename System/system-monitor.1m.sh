#!/bin/bash
# <xbar.title>System Monitor</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>stylo969</xbar.author>
# <xbar.author.github>stylo969</xbar.author.github>
# <xbar.desc>Displays CPU and RAM usage in the macOS menu bar.</xbar.desc>
# <xbar.dependencies>bash</xbar.dependencies>
# <xbar.category>System</xbar.category>

cpu_usage=$(top -l 1 | awk '/CPU usage/ {print $3}' | sed 's/%//')
used=$(vm_stat | grep "Pages active" | awk '{print $3}' | sed 's/\.//')
inactive=$(vm_stat | grep "Pages inactive" | awk '{print $3}' | sed 's/\.//')
wired=$(vm_stat | grep "Pages wired down" | awk '{print $4}' | sed 's/\.//')
compressed=$(vm_stat | grep "Pages occupied by compressor" | awk '{print $5}' | sed 's/\.//')
total=$(sysctl -n hw.memsize)
page_size=$(vm_stat | grep "page size of" | awk '{print $8}')
total_used=$(( (used + inactive + wired + compressed) * page_size ))
percent_ram=$(( (total_used * 100) / total ))

echo "CPU: ${cpu_usage}% | RAM: ${percent_ram}%"
echo "---"
echo "CPU Usage: ${cpu_usage}%"
echo "RAM Usage: ${percent_ram}%"
echo "Used Memory: $(bc <<< "scale=2; $total_used/1073741824") GB"
echo "Total Memory: $(bc <<< "scale=2; $total/1073741824") GB"

