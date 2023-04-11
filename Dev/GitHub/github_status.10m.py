#!/usr/bin/env python3

# <xbar.title>GitHub status</xbar.title>
# <xbar.version>v0.4</xbar.version>
# <xbar.author>Brett Jones</xbar.author>
# <xbar.author.github>blockloop</xbar.author.github>
# <xbar.image>https://cloud.githubusercontent.com/assets/3022496/12325555/a4b2bd9a-ba90-11e5-8254-9de54c2c6847.png</xbar.image>
# <xbar.desc>Shows the current status of www.githubstatus.com. Find out if Github is having DDOS problems which will affect pushes/pulls.</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>

import json
import time
from datetime import datetime
from urllib.request import urlopen

# See https://www.githubstatus.com/api
body = urlopen("https://kctbh9vrtdwd.statuspage.io/api/v2/status.json").read()
obj = json.loads(body.decode('utf-8'))

image = "PD94bWwgdmVyc2lvbj0iMS4wIiBzdGFuZGFsb25lPSJubyI/Pgo8IURPQ1RZUEUgc3ZnIFBVQkxJQyAiLS8vVzNDLy9EVEQgU1ZHIDEuMC8vRU4iICJodHRwOi8vd3d3LnczLm9yZy9UUi8yMDAxL1JFQy1TVkctMjAwMTA5MDQvRFREL3N2ZzEwLmR0ZCI+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgaWQ9ImJvZHlfMSIgd2lkdGg9IjE4IiBoZWlnaHQ9IjE4Ij4KCjxnIHRyYW5zZm9ybT0ibWF0cml4KDAuMTgzNjczNDcgMCAwIDAuMTg3NSAwIDApIj4KICAgIDxwYXRoIGQ9Ik00OC44NTQgMEMgMjEuODM5IDAgMCAyMiAwIDQ5LjIxN0MgMCA3MC45NzMgMTMuOTkzIDg5LjM4OSAzMy40MDUgOTUuOTA3QyAzNS44MzIgOTYuMzk2OTk2IDM2LjcyMSA5NC44NDggMzYuNzIxIDkzLjU0NUMgMzYuNzIxIDkyLjQwNCAzNi42NDEgODguNDkzIDM2LjY0MSA4NC40MThDIDIzLjA1MDk5OSA4Ny4zNTIgMjAuMjIwOTk5IDc4LjU1MSAyMC4yMjA5OTkgNzguNTUxQyAxOC4wMzY5OTkgNzIuODQ3IDE0LjgwMDk5OSA3MS4zODEwMDQgMTQuODAwOTk5IDcxLjM4MTAwNEMgMTAuMzUyOTk5IDY4LjM2NjAwNSAxNS4xMjQ5OTkgNjguMzY2MDA1IDE1LjEyNDk5OSA2OC4zNjYwMDVDIDIwLjA1ODk5OCA2OC42OTIgMjIuNjQ3OTk5IDczLjQxODAxIDIyLjY0Nzk5OSA3My40MTgwMUMgMjcuMDE1IDgwLjkxNDAxIDM0LjA1MiA3OC43OTYwMDUgMzYuODgzIDc3LjQ5MjAwNEMgMzcuMjg3IDc0LjMxNCAzOC41ODIgNzIuMTE0MDA2IDM5Ljk1NyA3MC44OTIwMDZDIDI5LjExOCA2OS43NTEwMSAxNy43MTQgNjUuNTE0MDEgMTcuNzE0IDQ2LjYwOTAwNUMgMTcuNzE0IDQxLjIzMTAwNyAxOS42NTQwMDEgMzYuODMxMDA1IDIyLjcyOCAzMy40MDkwMDRDIDIyLjI0MyAzMi4xODcwMDQgMjAuNTQ0IDI3LjEzNDAwNSAyMy4yMTQgMjAuMzcxMDA0QyAyMy4yMTQgMjAuMzcxMDA0IDI3LjMzOSAxOS4wNjcwMDMgMzYuNjQgMjUuNDIzMDA0QyA0MC42MjIwODYgMjQuMzQ1NjY1IDQ0LjcyODc2IDIzLjc5NzYxNSA0OC44NTQgMjMuNzkzMDA1QyA1Mi45NzkgMjMuNzkzMDA1IDU3LjE4NCAyNC4zNjQwMDQgNjEuMDY3IDI1LjQyMzAwNEMgNzAuMzY5IDE5LjA2NzAwNSA3NC40OTQgMjAuMzcxMDA0IDc0LjQ5NCAyMC4zNzEwMDRDIDc3LjE2NCAyNy4xMzQwMDUgNzUuNDY0MDA1IDMyLjE4NzAwNCA3NC45NzkwMDQgMzMuNDA5MDA0QyA3OC4xMzQgMzYuODMxMDA1IDc5Ljk5NCA0MS4yMzEwMDMgNzkuOTk0IDQ2LjYwOTAwNUMgNzkuOTk0IDY1LjUxNDAxIDY4LjU5MDAwNCA2OS42NjkwMSA1Ny42NzAwMDYgNzAuODkyMDA2QyA1OS40NTAwMDUgNzIuNDQgNjAuOTg2MDA4IDc1LjM3MzAxIDYwLjk4NjAwOCA4MC4wMTgwMDVDIDYwLjk4NjAwOCA4Ni42MTgwMDQgNjAuOTA2MDA2IDkxLjkxNTAxIDYwLjkwNjAwNiA5My41NDQwMUMgNjAuOTA2MDA2IDk0Ljg0ODAxIDYxLjc5NjAwNSA5Ni4zOTcgNjQuMjIyMDEgOTUuOTA4MDA1QyA4My42MzQwMSA4OS4zODgwMSA5Ny42MjcwMSA3MC45NzMwMSA5Ny42MjcwMSA0OS4yMTcwMDNDIDk3LjcwNyAyMiA3NS43ODggMCA0OC44NTQgMHoiIHN0cm9rZT0ibm9uZSIgZmlsbD0iIzI0MjkyRiIgZmlsbC1ydWxlPSJub256ZXJvIiAvPgo8L2c+Cjwvc3ZnPg=="
if obj["status"]["indicator"] == "none":
    print("✓ | templateImage=" + image)
else:
    print("✕ | color=red templateImage=" + image)

print("---")
print((obj["status"]["description"] + " | href=" + obj["page"]["url"]))

# convert UTC to local
utc_date = datetime.strptime(obj["page"]["updated_at"], '%Y-%m-%dT%H:%M:%S.%fZ')
now = time.time()
offset = datetime.fromtimestamp(now) - datetime.utcfromtimestamp(now)
local_time = utc_date + offset

print(("Last Change: %s" % local_time.strftime("%D %r")))
