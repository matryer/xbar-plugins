#!/usr/bin/env python
# -*- coding: utf-8 -*-
# <bitbar.title>GitHub status</bitbar.title>
# <bitbar.version>v0.2</bitbar.version>
# <bitbar.author>Brett Jones</bitbar.author>
# <bitbar.author.github>blockloop</bitbar.author.github>
# <bitbar.image>https://cloud.githubusercontent.com/assets/3022496/12325555/a4b2bd9a-ba90-11e5-8254-9de54c2c6847.png</bitbar.image>
# <bitbar.desc>Shows the current status of status.github.com. Find out if Github is having DDOS problems which will affect pushes/pulls.</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
#

import json
import time
from datetime import datetime

try:
    # For Python 3.0 and later
    from urllib.request import urlopen
except ImportError:
    # Fall back to Python 2's urllib2
    from urllib2 import urlopen

body = urlopen("https://status.github.com/api/last-message.json").read()
obj = json.loads(body.decode('utf-8'))

if obj["status"] == "good":
    # print("GH: ✔ | color=green")
    print("✓ | font='PilGi Regular' image=iVBORw0KGgoAAAANSUhEUgAAABgAAAAQCAYAAAF7I48DAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAIGNIUk0AAHolAACAgwAA+f8AAIDpAAB1MAAA6mAAADqYAAAXb5JfxUYAAARgSURBVHjaYt6/fz+Dr68vw9WrVw8xMDExfYqMjGRgYGD4DwAAAP//BMFBAQAwEMKwSprBU8MPbXjoEu7utUWFJGxD5QMAAP//Yrhw4QJECRL+//8/A9OVK1e079+/z/j48WNGBgYGBmZm5j8MDAwMAAAAAP//ZM2hEYAgGIDRj4PACtxJoHksYeNgNBormYkE7VSSC/wGowu8p5xzd61111rTez9SSmcphR8zxvgOQOac5JxprWGtfUQEE0K4vPey1lLGGGKMG8ALAAD//zyPsQqCUBiFv1sQxnWopF4hArdewAcQfIkGX8DBuUdoC+7uIAhtzvkU3gYX15ZwutLfUHamwwfng0NVVX+d1voJiFLq7Xnea+JlWSIiiAjzYRiubdseAZxzS75R4zgufh2ttUuS5A4ws9Yeoii6GGO2AE3TKGutAqjrWoVheOu6bj+N6fueOI7PQRA88jzfFEWxMsassyzb+b7/TNP0ND0WET58UTFLhlAUve8RKA0fukboM9pc1N8gNApOD/+ASzj7A5z6CTUIIoiTgwQNLraJS5A4tUUgTjaloLwWX0hEZzmXwz2He7hQFMWJEFLFcQxRFEHXdRCGISRJArIsv1ZV9bPMGAOglN7ycpIkfez8zrUgCG6OBjzP8xU/b5qmi50vubZt2zUcgPakf8EYQ3zGAAC2bT+0bYvSNEWu66Isy1DTNMiyrKc/A+q6BkEQvnzf9/M8B0rpnaIob/ypxw5nAACEENB1/dnzvEoURXAc53FZlnNN0/DvdDyOI1JVlZmmeV+W5ee6rqe+7zuM8YthGNswDOho+Cad/EGXCKA4/u64K/HuB78KlAYbft0ZaaAYLQ7XkIODiy5CDeogSU5tNgiC8VsaRA3iQBdBxEVQ4kQQEc7JQ7RB0YQLlKgO/1CYoHd6LfmD0iUa3vJ4jy/f9/08hOd5aDabj6PRaPPQpGm6FgwGnymKMsdx/I7BYLguiuJ6s9l8xnGcYlm2OJ1ObYf5VCp1QdP0J6fTeWw4kUiAw+GI/c3/v5TH43kaj8f/AOIKDI7jHiAIov6PgF6vlwRBOD8lgOh0urEkSVewaLXar2az+QOKoqgoijdlWT7b7/fYfr/fkiT5w2g0LpfLJTYYDB7udrvzw57Vam10u90nR6GtVqszAACKovrj8Rjp9Xq3TSZTgSCIj4IgPEomk/d8Pt/dQqFwv9FoOCVJ+ub3+98oinKj1WrdIknyOwDAYrHQn6Q2EAhcAoDKMEya53nIZDIXB+sul+tVqVSCbDYLlUoF7HZ79vdvzYvF4rVarQYWi+U9AKiRSOT5yQxmsxmEQqHXAKCGw+GXo9EIcrkcsCwL/X4f6vU6tNttKJfL0Ol0IJ1OA8dxMBwOwe12vwUANRaLBSaTycmQkdFoBLIsw3w+h2q1asrn8+8Wi4VVVdUVhmErgiB+4ji+22632Hq9JmVZJhEE0VIU1fB6vS8Yhvmi0WgARVGw2WxHF/o1AOIORIB/vrb+AAAAAElFTkSuQmCC")
else:
    print("✕ | color=red image=iVBORw0KGgoAAAANSUhEUgAAABgAAAAQCAYAAAF7I48DAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAIGNIUk0AAHolAACAgwAA+f8AAIDpAAB1MAAA6mAAADqYAAAXb5JfxUYAAAJOSURBVHjaYvzPAAeHGP4zMHz6z8DA8J+B4T8AAAD//2L4z8Bg8J+BgeH///8MjHBl//8zAAAAAP//gnGQtDIwMPz/z8jEwMCgzcDAwAjFDAwMDH8YGBgYAAAAAP//ZMwxDQAgEMDAPgKwhH8DBB0YKMvzIWHo0OUQVsIIQy33YwBablQR+1JT8OG6ygEAAP//gnH+Q/E7KP3vPwPDZ7g41F6YhllIGnDhKpgGJgYGBk0GBobJDAwMomjug7E3MTAwqCM8A8HN/xkY7vxnYBD6z8Ag8J+BQfA/A4MY1IkpyE4CAAAA//9s0rEJAmEMhuEHucrCAcRKdBYXcAcbJ3EF5xAsbgvdQBzAykrOwuh9egZCQvhDvuT9X8vQxnLpp3z8btiG1mvFS9RW2TDCPM49rTiL2iJ5DAn9s6KW5PZf9Ho/Dpt7mveOTeW7uppfcI38s7SVHzAOBR9ranSHNW6Y4IwlHqkfnozTsUpDUQwG4K+LgyjUOgqCoos4CJ0cxaGPIU6Cm6O7+AIWnBx8hw6d+gYdBDcHR4soiugiSBwMej3cqz2QITkJyZ/8/w9Pf1NhGCzm33KwHiylvxaMi/yV8l4lhJ1i0h4eTP+2cVt/ETanIPd/dh+06xC0gpuCLBNc5QE6mM/dv+MFT+l30a7UjUTs1iGY5BTXFQ3sBf34ImZVGwvBZdBLvxM8f9fXIBCcZsJZFq1WoB8XDS4y/hjMZGyQsYOmBoKTTDpqUHWT9bNuv4lFrUKYGzjHFl7T3vCRe59Lm8UIhyLu/qLX5wCSNr11XIiJWQAAAABJRU5ErkJggg==")

print("---")
print(obj["body"] + " | href=https://status.github.com/")

# convert UTC to local
utc_date = datetime.strptime(obj["created_on"], '%Y-%m-%dT%H:%M:%SZ')
now = time.time()
offset = datetime.fromtimestamp(now) - datetime.utcfromtimestamp(now)
local_time = utc_date + offset

print("Last Change: %s" % local_time.strftime("%D %r"))
