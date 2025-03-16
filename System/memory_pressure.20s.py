#!/usr/bin/env python3

# <xbar.title>Memory Pressure</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Martin Trojer</xbar.author>
# <xbar.author.github>martintrojer</xbar.author.github>
# <xbar.desc>Memory Pressure Indicator</xbar.desc>
# <xbar.image>https://github.com/martintrojer/xbar-plugins/blob/a4fca3de82074febc8751bab0ce2d63e540cf04f/mem_pressure.png?raw=true</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl></xbar.abouturl>

import subprocess

try:
    stats = subprocess.check_output(["memory_pressure", "-v"]).decode("utf-8")
    for line in stats.splitlines():
        if "System-wide memory free percentage" in line:
            mem_used = line.split()[-1].replace("%", "")
            pressure = int(100 - float(mem_used))
        elif "Pages free:" in line:
            free_pages = int(line.split()[2])
        elif "Pages active:" in line:
            active_pages = int(line.split()[2])
        elif "Pages inactive:" in line:
            inactive_pages = int(line.split()[2])
        elif "Pages speculative:" in line:
            speculative_pages = int(line.split()[2])
        elif "Pages wired down:" in line:
            wired_pages = int(line.split()[3])

    used_pages = active_pages + inactive_pages + speculative_pages + wired_pages

    if pressure < 70:
        icon = "🟢"
    elif pressure < 85:
        icon = "🟡"
    else:
        icon = "🔴"

    print(f"{icon} {pressure}%")
    print("---")
    print(f"Memory Pressure: {pressure}%")
    print(f"Free Pages: {free_pages}")
    print(f"Used Pages: {used_pages}")

except Exception as e:
    print(f"An error occurred: {e}")
