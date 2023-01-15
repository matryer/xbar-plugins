#!/usr/bin/env python3

# <xbar.title>Who's Home? (for UniFi)</xbar.title>
# <xbar.desc>Quickly see all smartphones on your UniFi network. Set custom aliases for devices and access points. Optionally get notified when phones connect/roam/disconnect.</xbar.desc>
# <xbar.abouturl>https://github.com/DouweM/xbar-whos-home-unifi</xbar.abouturl>
# <xbar.author>Douwe Maan</xbar.author>
# <xbar.author.github>DouweM</xbar.author.github>
# <xbar.version>v1.0.1</xbar.version>
# <xbar.image>https://i.postimg.cc/j5CMVk8d/whos-home-unifi.png</xbar.desc>
# <xbar.dependencies>python,pyunifi,aiohttp</xbar.dependencies>

## Dependencies:
# - https://www.python.org
# - https://pypi.org/project/pyunifi
# - https://pypi.org/project/aiohttp

## Settings: xbar menu bar item > xbar > Plugin browser...
# Controller
# <xbar.var>string(VAR_CONTROLLER_HOST="192.168.1.1"):      Host of UniFi Controller</xbar.var>
# <xbar.var>string(VAR_CONTROLLER_USERNAME="admin"):        Username for UniFi Controller</xbar.var>
# <xbar.var>string(VAR_CONTROLLER_PASSWORD="admin"):        Password for UniFi Controller</xbar.var>
# <xbar.var>number(VAR_CONTROLLER_PORT=443):                Port for UniFi Controller</xbar.var>
# <xbar.var>select(VAR_CONTROLLER_VERSION="UDMP-unifiOS"):  Version of UniFi Controller [UDMP-unifiOS,unifiOS,v5,v4]</xbar.var>
# <xbar.var>boolean(VAR_CONTROLLER_SSL_VERIFY=false):       Verify SSL connection to UniFi Controller</xbar.var>
#
# Aliases
# <xbar.var>string(VAR_ACCESS_POINT_ALIASES=""):    Aliases to use for access points: `<Name/MAC>=<Alias>`, separated by `;`, e.g. `Dream Machine=Living`. (Device aliases can be configured in the UniFi Network UI, e.g. set device name to `Douwe's iPhone` to show as `Douwe`.)</xbar.var>
#
# Avatars
# <xbar.var>string(VAR_AVATARS=""):                 Set Gravatar emails or image URLs for devices to enable avatars: `<Name>=<Email/URL>`, separated by `;`, e.g. `Douwe=hi@douwe.me`</xbar.var>
# <xbar.var>string(VAR_CLOUDIMAGE_TOKEN=""):        Set Cloudimage.io token to show round avatars instead of square</xbar.var>
#
# Event notifications
# <xbar.var>boolean(VAR_MENU_BAR_EVENTS=false):     Show connect/roam/disconnect events in menu bar</xbar.var>
# <xbar.var>string(VAR_TERMINAL_NOTIFIER_PATH=""):  Set path to terminal-notifier to show notifications for connect/roam/disconnect events</xbar.var>
# <xbar.var>boolean(VAR_NOTIFY_CONNECT=true):       Notify when a device connects</xbar.var>
# <xbar.var>boolean(VAR_NOTIFY_ROAM=false):         Notify when a device roams (moves) between access points</xbar.var>
# <xbar.var>boolean(VAR_NOTIFY_DISCONNECT=true):    Notify when a device disconnects</xbar.var>
#
# Debugging
# <xbar.var>boolean(VAR_SHOW_TEST_DEVICES=false):   Show dummy devices for each configured avatar instead of real devices</xbar.var>

from contextlib import contextmanager
from datetime import datetime, timezone
import json
from math import floor
from pathlib import Path
import random
import subprocess
import tempfile
import aiohttp
import asyncio
import hashlib
import base64
import re
import os
from collections import defaultdict
from pyunifi.controller import Controller, APIError
from yarl import URL

MENUBAR_ICON_B64 = "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"

b64_prefix = "iVBORw0KGgoAAAANSUhEUgAAACIAAAAiCAQAAACQTsNJAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAAmJLR0QAAKqNIzIAAAAJcEhZcwAAFiUAABYlAUlSJPAAAAAHdElNRQfmDB4A"
MENUBAR_NUMBER_ICONS_B64 = {
    0: "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",
    1: f"{b64_prefix}Mh5ugPaAAAAAAW9yTlQBz6J3mgAAAqZJREFUSMfdk01r1FAUhp+b3CSdzIx1oFCtBb9AF0JRClppF4IoflMRnboRwYVLFVeCf8KFUEF04Y9wp/4ClX5YwSq4VBARp9NOppnXRTJfnelMBVeekHBv7nue3POeG2gN0zbyYdsZ+9v5FZxOZlsJA2eDwX3s3bMjSTow7QohVLgAeFtCDOXtkhFlK3sf9hdDIRMRIVc7L28FY+B4JvMZUXU0UjxyMhAyFYSoIE+jV7aC8SB8gogpDT/ICJk1NDDvzycjI7fYH2OB54gqMSnCfDmYH8qbL4g1hOiLscAzRBVRo4LM8mABYM/2zCfEKqLvbizwNIVUUGFpfBCwWJjM5xbrGGZ6YQLgBWKdCGWWpwoNsYWxbLDQwBQ3Ozc+uNfrvch/mMwDFhcHB3cjJnet2258cGdQYmC4eCIH2LYTbGE89OcTjKddHRgP/CJKBNkE0TWaGKuR6daiPHAbiIHFsWyvFo6Hdh6ZVeSqcLGO8SDTQIQLY9m0T/V4jRALzQ6Oh95cojYKz6dFDV2yqZ1BJ+JO8gM2IGBhNGPnkgwj9xwQnDFCRCh4dyjXhrA8pNYBAQsjoX2bZKHsKbwfyETEKHOsxagst1lMARsh4IN3FBGbCIXfrRtVkUcNVg0Qp7KrzHb11aBEVXUAyQOtO4Vr3hv7EadNWp+t8KgD0hqu+ey9ys0k41sIMQG46X2TFWYZ5XBHOXXFBELOvcSkgEocdGz7JcOUgCF6Rs0HAksMqH0F+LZ5XttMQOx0kWlD5Z3GbnjndJVqU0jXFYd/EP8ZxPZZf9+jU10hDuCmSes99S5xaw2tkBIQ9fnoeuNZAtKWNyHiMV/x206CKBMCK2TbyjJE7EYY3CZEgJhi6q/8rGGaaAvcQPykRI1y36tGmVVKiLtJ9h9VdSnLjFBUtQAAAIRlWElmTU0AKgAAAAgABQESAAMAAAABAAEAAAEaAAUAAAABAAAASgEbAAUAAAABAAAAUgEoAAMAAAABAAIAAIdpAAQAAAABAAAAWgAAAAAAAACQAAAAAQAAAJAAAAABAAOgAQADAAAAAQABAACgAgAEAAAAAQAAACKgAwAEAAAAAQAAACIAAAAAEEJAwQAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMi0xMi0zMFQwMDo1MDoyOSswMDowMMZvJ60AAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjItMTItMzBUMDA6NTA6MjkrMDA6MDC3Mp8RAAAAEXRFWHRleGlmOkNvbG9yU3BhY2UAMQ+bAkkAAAASdEVYdGV4aWY6RXhpZk9mZnNldAA5MFmM3psAAAAXdEVYdGV4aWY6UGl4ZWxYRGltZW5zaW9uADM0Yc/CIgAAABd0RVh0ZXhpZjpQaXhlbFlEaW1lbnNpb24AMzS8WRunAAAAAElFTkSuQmCC",
    2: f"{b64_prefix}Mg3qPrdeAAAAAW9yTlQBz6J3mgAAAxJJREFUSMedlNuLFEcUh7/qrp52emayGVhRVyVeMAZCFnTBC67xGlETxSBk9EUIIeQ1QV98l5B/IATFy4Pgn+CT5kJInryA62YC2SiCbIhgVJzspWd7fnnomp6Z3dmd3ZyCpqpOna/q/M6hod1MxywHbxy2r71X4aF0tRgzcCTs28D6dSvToLdP+EIIlT8CgkUh+ku2asSElT0LGyuRkImJka9VHy8GY2BnPv8nou5poLLlQChkphFiGgVac3IxmACii4iE2orzeSEzhZaN5EbSmZFf6Y2xwDVEnQSHMI82l/pL5hFiCiF6YixwFVFHNJhGZqyvDLDuzfwfiElEz9dY4LKDTKNydagPsFjYVSqONjGcWggTAtcRM8QoPzZczg5bGCyEDzNMZb6+yYF/ulmL0m+7SoDFx8PDn40pftLtNTnwT6FUwGh0bxGwHR1sYSjKjaSYQKvnYALIVVB6oJAiuloLYzVwoj2pAPwMsWx0sLBQCYciO4LMJPJVPtbEBJDPENHDwYKrU8g5fuUZdV5S5Ts2NSUeioIH6Wmj6EOXVP9x6+QMW4gSd1DHmOJAE7Mmbx+kEUb+USA8bISIUXj/3aJDwDdZ8BNXVvHUKWBhILL30ihU+IDgOTIxCcpvbxPqLxf4OTDAU7fa06plsA2RmBhFzzw/BgUAkwZIACjzmDFe8Q+XgXF+dMFlV/QE6h4gBaAZVu0OfrBVhNgB+AB4LihdlRh3L3nHeXxgB0JmLLi9/P1077MOiN9R0yK3HOKnDJBBvK/SO0NIwnnbYgO/uKq85Iu57kYOCD0SQJ2ebHaIOwwC8DcH+X2WFxeZeHPpyMn3KTcpA/AzW7kLgJl1IYDpBkn5Z7ji1PmafYy3eeaYnUeL97jk3vM9VU673fuMLgVygabY+9mf7Z7vDumezlqOswTrDtm9FMR86dzgxv+FeIDv5JxZ8LxP0p5DO6QGxD0uncm+NcCVvAUR3/KEXEcniAki4F8KbT9tMMS8hTBpJ9mMJ4YZXpKeDUwLbYEziBfUaDDRczSYYJIa4ss0+j/opE38m5vTIQAAAIRlWElmTU0AKgAAAAgABQESAAMAAAABAAEAAAEaAAUAAAABAAAASgEbAAUAAAABAAAAUgEoAAMAAAABAAIAAIdpAAQAAAABAAAAWgAAAAAAAACQAAAAAQAAAJAAAAABAAOgAQADAAAAAQABAACgAgAEAAAAAQAAACKgAwAEAAAAAQAAACIAAAAAEEJAwQAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMi0xMi0zMFQwMDo1MDoxMiswMDowMErndLQAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjItMTItMzBUMDA6NTA6MTIrMDA6MDA7uswIAAAAEXRFWHRleGlmOkNvbG9yU3BhY2UAMQ+bAkkAAAASdEVYdGV4aWY6RXhpZk9mZnNldAA5MFmM3psAAAAXdEVYdGV4aWY6UGl4ZWxYRGltZW5zaW9uADM0Yc/CIgAAABd0RVh0ZXhpZjpQaXhlbFlEaW1lbnNpb24AMzS8WRunAAAAAElFTkSuQmCC",
    3: f"{b64_prefix}MTcHHz0vAAAAAW9yTlQBz6J3mgAAAzlJREFUSMeNk8uLHFUUh79bdasrVT3t2GGCZjJifI0LYYKMaGJGDRpjfBIR0nHjxp0uNOhGwbVuBBUSiIgudJG/QYkPVFypIZ3JLBwjgYFAgiaanu6Z6sfPRd2urp7pnu5TUNx77jnfPa8LeTF9qwLcdNje8P4ND6W7ccTA0+Hkndyx+9bUafaIL4RQ+TkgGAsxVbJLRtSt7FtwVyUWMgkJ8rXzxXEwBvZF0Z+Ipqfpyv1PhEJmHSHWUaCZl8bBBBCfQrSp3fJOJGTW0LZqoZqujPzKaIwFvkA0aeMQ5uK9pamSuYhYQ4iRGAt8jmgiOqwjszxZBth9c/QHooEYGY0FPnOQdVRemp8ELBb2lyYWuxiObYUJgS8RLRIULS+UM2MLc8XwfIapDJubAvgvd3tRurC/BFh8PDz8jZiJo4OiKYB/DKUFjBcPTAC2b4ItzMeFaooJtGsTJoBCBaUGxRQxUHoYq+kj+aQC8DPEtsW54lYtnI9tFZkG8lV+vosJIMoQ8fm5outTxNv8wnVa/MdZPmBHt8TzcXAutTaKn3VJTb1gXTnDHqLIT6jvW2Gmi5mJ7LnUw8h/BggPGyESFP5+34RDwMfO9SyfsuLWp3rzNB3b31IvVHyS4G9kEtooeigrlOFrVhGXiYB9DvJjvpfBg4i2SVB8xfpJEwV0oGGANgDiEB6zbKcBzDrXC4BBqVXTA6QA1GLnI8F3dgkh9gI+AF5251G+SZ8jl9iVnfjAXoTMcnBmx6Op7tU+iJ/r6IculTUqTuPnId7x9M4Q2uHQsdievavTnNh83CkAoUcbUP9Jbv0eMQe5AcBrPLzhNPVse5vpKPdmVmhwhq/c7oArbL8YOzCJEo9zD3dT5zgA/zh9awAiG62N4nOaEEj4hL/weMrpfx1cOG+g9pqbzgI/8BE/8wAAVb4dDBkcCbzLHh4DbuMNp7lMZXAywyKBVQ7yOt9zlSbXqfI+e1gaYjs0EmhxkpOMJXmIB/iuva0t7X3a+RzykBqQjLi0lf1rgBvTHkSc4BKFvuKJOjGwSjE3gGBIuB1h0ndmM55YYGG8GjjpYHpoC7yCuEaNDvWRX4c6DWqIN1Pv/wF0NFq4HfyIWAAAAIRlWElmTU0AKgAAAAgABQESAAMAAAABAAEAAAEaAAUAAAABAAAASgEbAAUAAAABAAAAUgEoAAMAAAABAAIAAIdpAAQAAAABAAAAWgAAAAAAAACQAAAAAQAAAJAAAAABAAOgAQADAAAAAQABAACgAgAEAAAAAQAAACKgAwAEAAAAAQAAACIAAAAAEEJAwQAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMi0xMi0zMFQwMDo0OTo1NCswMDowMJDmu34AAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjItMTItMzBUMDA6NDk6NTQrMDA6MDDhuwPCAAAAEXRFWHRleGlmOkNvbG9yU3BhY2UAMQ+bAkkAAAASdEVYdGV4aWY6RXhpZk9mZnNldAA5MFmM3psAAAAXdEVYdGV4aWY6UGl4ZWxYRGltZW5zaW9uADM0Yc/CIgAAABd0RVh0ZXhpZjpQaXhlbFlEaW1lbnNpb24AMzS8WRunAAAAAElFTkSuQmCC",
    4: f"{b64_prefix}MSSDoXzxAAAAAW9yTlQBz6J3mgAAAuRJREFUSMetk8+LFEcUxz/VXd3t9My4GVhJshHcKEQhsCALurJLCAQTf7NBdNZLLoJnJaeQox78AxQURA9evIRAbjkk+BcYcF03IetPPAkqksmu2zs93xyqZ6Z7Z9yZhbymu+tVvfep9+q9gryYwiiErYfsP97b6GunDSMGDkcjO/l0/CPn9NmsL4RQ7RgQDIUYrdpFI5at7Pewqx4LmYQE+fr422EwBg6USo8Qa57G6nu/ioTMKkKsokDbTw6DCSC+jkhpfPhDSci8Q1vmw3k3MvLrgzEWuIVYIyVDmMe7q6NV8xjxDiEGYixwE7GGaLGKzNJIDWD8g9LfiBXEwGgscCODrKLa4uQIYLEwXa0stDHMbYSJgNuIJgkqLc3UOsYWJsrRgw6m/r6+CcE/065F9eF0FbD4eHj46zGV0/2iCcGfQ+4A44UvK4AtdLCFyTicd5hAn/RgAgjryBmUHaKvdDFWY7P5pALwO4gtCxPljUo4Gdt5ZFaQr9rxNiaAUgcRP5goZ3Xq1uw3d38YdyuTcXDfWRvFR7OkRk/Y7DijXgRcyRAOAha2l+x952HkHwGiQ0aIBEV/fF7pQZzrINoQsDAW23vOC5UPErxCJiFFpf091f/Cma2DQAjBPkRqEhS/9PwEFACsGCDNIcb5iQD4Kzfnip7CmgdIAajp1U4Hd+2feIUIPKDCL4wCv3KxB9IW3zwKfq/MufFZhJgC/Ow1/IwQi4wwV0inbTGFkHfB7RlBGvW0w0Vmgdcc5+1GXdMKgcgjBVRc4SQ/Ak1OsdTrV9AEpLYPXhzstFlenvCKbes2BDAemxP1m9wspK/YvrOXuJbTvuEyAEd5vhnIC17ktD3Z/yFP+0P+l3SGgdzBYDDvi6OYjgf4WWM3N7T3SfPb5yENIBkQVbPzbQBZybsQcZVnhIVOEMvEwL+UC1fPkLADYfC7EAFihplNnWcL00Vb4DvEGxq0WB74tFhmhQbivPP+D4iLNOI/Btu1AAAAhGVYSWZNTQAqAAAACAAFARIAAwAAAAEAAQAAARoABQAAAAEAAABKARsABQAAAAEAAABSASgAAwAAAAEAAgAAh2kABAAAAAEAAABaAAAAAAAAAJAAAAABAAAAkAAAAAEAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAIqADAAQAAAABAAAAIgAAAAAQQkDBAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIyLTEyLTMwVDAwOjQ5OjM1KzAwOjAw8P65TQAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMi0xMi0zMFQwMDo0OTozNSswMDowMIGjAfEAAAARdEVYdGV4aWY6Q29sb3JTcGFjZQAxD5sCSQAAABJ0RVh0ZXhpZjpFeGlmT2Zmc2V0ADkwWYzemwAAABd0RVh0ZXhpZjpQaXhlbFhEaW1lbnNpb24AMzRhz8IiAAAAF3RFWHRleGlmOlBpeGVsWURpbWVuc2lvbgAzNLxZG6cAAAAASUVORK5CYII=",
    5: f"{b64_prefix}MRCiFYhEAAAAAW9yTlQBz6J3mgAAAy5JREFUSMeVk91rHFUYxn9nzpmZzuxu40pAm0YatWhBCEhsiDQXgijxu0XoxguLoPcqeqP/Q0EvWqiIXoi33mi9EaI3pRashW5iKrZFLV5oaYt23W12d/bxYmY/JtlN1meYnTN7nvc357zve2BQJjcKYPeSu+39HT6dvo0jA8+EEw9w/8y9adBDh60QQuXnAX8sxGTJrRtRd3LvwIOVWMg0aSKrPUfGwRh4PIquIFqepiqPPhkKmQ2E2EC+pl8eB+NDfAqRULvnvUjI3EG7qkE1HRnZys4YB3yKaJGQIczVh0uTJXMVcQchdsQ44BNEC9FhA5nLE2WAmbuiXxANxI6rccDHGWQDldfnJgCHg0Ol4loXw/J2mBD4DNGmiaLLi+We2cFsIVztYSqj+iYA+0q3FqWfDpUAh8XDw27GFI8OW00AdhmlCYzXnigCLtfBDubioJpifO3dgvEhqKDUUEgRQ9XHOE0dHtyUD7aH2LU2W9iuhHOxqyLTQFblF7oYH6IeIl6dLWR1+hxtun5NZ+Zi/2LqNoqfyzY1+aLL0hn2EXB+KAQcTEfuYhphZJ8FwiUjRBOFFx4p9hBwewQEHEzF7sc0ChWeMv6N1t2mJYsXLTTOEdAE4D5+B+Ajvuylo85Kr5ZNf751jo5J5MfXnW22kE8HGgZIMtuB7Hmar3J5NSh1tTxA8kFtr3zU/85dwstZvR7kJarUuclpFjPIoKy54q8Ul9Px6wixANjsPrElI21eTc3ZvYCQ93b6zRCScEs7dFdS5SQXsuBT7Nls6wRA6EgA5WeA1zjAfiI+JMFyloNAxBFO0sl5BSSOrRKGa1zjm+w94WsOArAvS2xeZhgELHuZZpoWX+TSeX0IYqC18trPOgD/cIY/8VjK/v92qHtTabu6lLXYbr7nA84wD8AK5/8PBN5gFYAZ3mQBgJ85NsI7EvIX87zLWW6RcJMfeJ/H+GMUxI2aoMFxjjOWBiEeYLNKtLf1W5LBPQxCapCd4dFq935rQNamfYg4wW8EuU4QdWLgXwq5o2dosg9hsH2IALGYndRx1cH00Q44hrhFjQ71Ha8OdRrUEG+l0f8BZTxhY7LL2zAAAACEZVhJZk1NACoAAAAIAAUBEgADAAAAAQABAAABGgAFAAAAAQAAAEoBGwAFAAAAAQAAAFIBKAADAAAAAQACAACHaQAEAAAAAQAAAFoAAAAAAAAAkAAAAAEAAACQAAAAAQADoAEAAwAAAAEAAQAAoAIABAAAAAEAAAAioAMABAAAAAEAAAAiAAAAABBCQMEAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjItMTItMzBUMDA6NDk6MTUrMDA6MDCy274wAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIyLTEyLTMwVDAwOjQ5OjE1KzAwOjAww4YGjAAAABF0RVh0ZXhpZjpDb2xvclNwYWNlADEPmwJJAAAAEnRFWHRleGlmOkV4aWZPZmZzZXQAOTBZjN6bAAAAF3RFWHRleGlmOlBpeGVsWERpbWVuc2lvbgAzNGHPwiIAAAAXdEVYdGV4aWY6UGl4ZWxZRGltZW5zaW9uADM0vFkbpwAAAABJRU5ErkJggg==",
    6: f"{b64_prefix}MDiOuxH/AAAAAW9yTlQBz6J3mgAAAzhJREFUSMeNk8tvG1UUxn935o4nM7YJFpFKQqWWh4popUhgCVo1CxAPlUeggBQnC7rpquqmCAmJBRtWiB0LFgUEQqxQi1QJVoiX+AMoahJa1LTAAooCFVAcO3Yy/ljM9WTs2onPSPa58jk/n/udbyAfpicrwC1H7H/ev+ET6WmUMPBkOH4Xd+69PW3ad9QXQqjyDBCMhJgo24tGNKzsK3B3LRYybdrI1+Tzo2AMHIqiK4gNT1O1+x8NhUwLIVoo0O4XR8EEEJ9GJNR3vRYJmXU0tlhYTDMjv7YzxgIfIjZIcAhz9d7yRNlcRawjxI4YC3yA2EB0aCGzMl4B2HtrdBnRROw4jQXed5AWqlysjgMWC4fLpeUuhvntMCHwMWKTNopWZipZsYXpYriUYWrDfFMAf6G7i/KPh8uAxcfDw+/HlOYGTVMAfx6lAsbLD5cA2+NgC9W4sJhiAt1xEyaAQg2lBcUUMTC2MFZTR/OXCsDPEGPL08XtVliN7SIyTeSrMtvFBBBliHhpuuj2BLt4iyXqNPmFj9jflbgaBxfSaqP4aXepiWetkzPMIw5xHeWeBo91MbsjeyHtMPKfAsIjRog2Cs8fKGWI2/gDITp8wmcO85tTwMJUbL9Pu1DxcYLryLRJUPRQTqjXXeMbAHzHT3zBu+zZ2mXwICIxbRSvMnYNIRLEQcB3Zecd5J4+XdOl+8BBxCZC0e9eZS741l7C6/PuAQBaGM7yD2t8wyM5SDd8cyX4ujSf5sdRbhKfSTfHWk7chJeyKbqTyHsZwCOEJOwbumu2mHHO8Dlp5Wkm+13TKQChRwKo9xfaWX6SOWZ5E4CIeaDTUysg8bg5xGqGPQvAGXe6D9P3hwBmEASaXHZZ6prEndYHIIDBEDjnvo8BMOtOPwypxgInerYDMMGfzrHn+JQOQqxmgmfb4VUgGDbJXyxwAzA8xwsYYI0F6oOLh0HgS6q8zSVusM7PvMcDfLXdZYbFCqcYKfIQD/CdsTe3rfdJ8nfIQ+qQs9ng2Mw+U33UCxHv8CuFHieIBjGwRrHn1TO02YMw6TZtxhMzzIymgYsOZgttgWOIv6nTobHj06FBkzriVNr9PwCuX8hVNtxtAAAAhGVYSWZNTQAqAAAACAAFARIAAwAAAAEAAQAAARoABQAAAAEAAABKARsABQAAAAEAAABSASgAAwAAAAEAAgAAh2kABAAAAAEAAABaAAAAAAAAAJAAAAABAAAAkAAAAAEAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAIqADAAQAAAABAAAAIgAAAAAQQkDBAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIyLTEyLTMwVDAwOjQ4OjU2KzAwOjAw6LvBaQAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMi0xMi0zMFQwMDo0ODo1NiswMDowMJnmedUAAAARdEVYdGV4aWY6Q29sb3JTcGFjZQAxD5sCSQAAABJ0RVh0ZXhpZjpFeGlmT2Zmc2V0ADkwWYzemwAAABd0RVh0ZXhpZjpQaXhlbFhEaW1lbnNpb24AMzRhz8IiAAAAF3RFWHRleGlmOlBpeGVsWURpbWVuc2lvbgAzNLxZG6cAAAAASUVORK5CYII=",
    7: f"{b64_prefix}MApGbEB/AAAAAW9yTlQBz6J3mgAAAulJREFUSMel1M9rHGUcx/H3M/PMTGY2a7o2oo2RxhbagxAIgZLSKGJR6k8iATdSSA9611IQ9KL4P3gQSnsoPfTQi6deiicP7cFA0hjBGFBBDyIibjbNbmY/Hp7ZyUyy2Q3mO7DzzH6f5zXPz4FimFIphCcu2X+9f6LX3NNhwsDr0cgpnp94xjU6M+cLIVR7CwgORYxW7ZoRTSt7DU7XEyHTooV8nXj3MIyB83H8M6Ltaaw+dTESMtsIsY0Cjc8fhgkg+RqR0nj601jIPEZDK+GKKxn59cGMBW4i2qRkhNk4Wx2tmg3EY4QYyFjgBqKN6LCNzPpIDWDiWPwTYgsxsDcWuJ4h26i2Nj0CWCxcqA6vdhkW+jERcAuxQwvF67O1vLKFyUr0KGfqB+2bEPz3u2tR/eFCFbD4eHj4e5nh93r1JgR/AbkJTFZfHgZsaQdbmE7CFccEenYfE0BYR65CxRE9Y5exGpsrDioAPyeGVicr/ZZwOrEryGwhX7W3u0wAcU4kjyYrwATqeX3hBhUsu9pGyZvZoEbfsdl0Ro6wjB+IgIXx2C67Fkb+G0B0yQjRQtHSC8PZbjkI+ay74GOJ/d61QpVXTfBX+0nTlo8Xz2w9IKQFVPmoMA0n+RCAh1ykka1lKzjXfkDHpAqSPxn6AyFSxAzg75tJn+8Q4jeO0/1s+cAMYgeh+HdOvBh8a9dQCfEKyCcI0eGVQqaLyKwH9596yf33QQkp9uY5mghxp9CzAuJddXIEaXTgtviSGEj5vHe6EwKRRwqonMlL41wG4B4/9siStUy9HrzyM7OYnY7bec7seSGA6YXs9mwegDbf7MuUojfi4jhTACyxSd/oh5zLhvUQ/j8yld03joKcze6/HgW5gsFguDsIsXtAP5uHnb71fdLi64tIA2gNeOlO/uvOs8qI+IpfCEs7QTRJgE0qhY82GFqcRBh3zmzuiVlmB42+FB3MLm2BRcTfNOjQHHh1aLJFA/Gxa/0fkVpL5ZBqUCMAAACEZVhJZk1NACoAAAAIAAUBEgADAAAAAQABAAABGgAFAAAAAQAAAEoBGwAFAAAAAQAAAFIBKAADAAAAAQACAACHaQAEAAAAAQAAAFoAAAAAAAAAkAAAAAEAAACQAAAAAQADoAEAAwAAAAEAAQAAoAIABAAAAAEAAAAioAMABAAAAAEAAAAiAAAAABBCQMEAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjItMTItMzBUMDA6NDg6MTArMDA6MDAPIfqpAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIyLTEyLTMwVDAwOjQ4OjEwKzAwOjAwfnxCFQAAABF0RVh0ZXhpZjpDb2xvclNwYWNlADEPmwJJAAAAEnRFWHRleGlmOkV4aWZPZmZzZXQAOTBZjN6bAAAAF3RFWHRleGlmOlBpeGVsWERpbWVuc2lvbgAzNGHPwiIAAAAXdEVYdGV4aWY6UGl4ZWxZRGltZW5zaW9uADM0vFkbpwAAAABJRU5ErkJggg==",
    8: f"{b64_prefix}Ly/AMpqmAAAAAW9yTlQBz6J3mgAAAz9JREFUSMeVlEuIHFUUhr9bdasqXd3tTMtgdAwYNRgkyWAc0EhmIfggvsIEIT1BcKEbwSwShYDoWhcuXIhIRMxCQRAEHwFXSnAjLnwwkzGCSQZRSQiGMUzPdLq6q38Xdfs13TPdOUVVn6o65zv3nvNXQ7eZHi+Emw7YFe9a9Hh2N4oZeCIau4s7t9+aJd0z6wshVHoaCEZCTBTtOSPWrOyrcHc5FjIJCfJ126FRMAYeyuUuIOqeJst7H4mETA0haijQtmdHwQQQn0SkVLa+lhMy19GWhXAh84z88nCMBU4h6qQ4hLm4szhRNBcR1xFiKMYCHyHqiCY1ZM6PlQC2j+f+QFQRQ1djgQ8dpIZK56bHAIuF/cXCYgvD3GaYCPgY0SBBufMzpXawhal8dLaNKW+kmxD8I61ZFH/bXwQsPh4e/npM4fCg1YTgz6GsgfHiwwXA9ijYwnQcLmSYQLf3YQIIyygLyGeIgdbBWE3Odm8qAL+N2LI4ld9shNOxXUCminyVnmlhAsi1EfHZqbybk88RvuVvEv7jJ97kllaLp+NgPos2ip9ym5o4aF07ow7Cchr1HFfY1cJsy9n5LMPIfxKIDhghEhT9sqvgEPCyS13iHX5w/vcdPU3G9ucsC+UfI7iKTEKKcg92NeoLl7gDCFhCiKYrACEEDyBSk6D4iucnoACgaoDUha263xpQpwrAMg039BTqHiAFoIZXOhycsb/j9YzA41PnfcPrfM69ALxL938fgG8uBN8V5jL/RYTYB/juhFeo9TT2lCvUitiHkHc8qxlBGvXJIeb+dbKe5dF+1TRDIPJIAfW+AT7gOQzwFuPMkgDjfMa4e9sxAanXT0cUyfZ5mTe4xpd8AsAYhzDrCgKYQRDY6vpy1dW97J5PDkAAgyGXXPJO9gAhB93zpYHRG0BWOQ2A5Qzv8yO73bq+vhEIHOUfAG7mJe4DoM4LrNwY5C/28DbzrNDkX37lJHv5aoPY9tfQb8uc4AQjWTfEA3wn7Mam8T5p9x66IRUgGVK00b5WACfTDkS8x5+EPUoQa8TAKvmeT8+QcAfCZHqybZ6YYWa0HjhrYjpoCzyPWKZCk7WhR5M1qlQQx7Ls/wFvw1d4y02SsgAAAIRlWElmTU0AKgAAAAgABQESAAMAAAABAAEAAAEaAAUAAAABAAAASgEbAAUAAAABAAAAUgEoAAMAAAABAAIAAIdpAAQAAAABAAAAWgAAAAAAAACQAAAAAQAAAJAAAAABAAOgAQADAAAAAQABAACgAgAEAAAAAQAAACKgAwAEAAAAAQAAACIAAAAAEEJAwQAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMi0xMi0zMFQwMDo0Nzo0NyswMDowMHNtkc4AAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjItMTItMzBUMDA6NDc6NDcrMDA6MDACMClyAAAAEXRFWHRleGlmOkNvbG9yU3BhY2UAMQ+bAkkAAAASdEVYdGV4aWY6RXhpZk9mZnNldAA5MFmM3psAAAAXdEVYdGV4aWY6UGl4ZWxYRGltZW5zaW9uADM0Yc/CIgAAABd0RVh0ZXhpZjpQaXhlbFlEaW1lbnNpb24AMzS8WRunAAAAAElFTkSuQmCC",
    9: f"{b64_prefix}LwPy6vZFAAAAAW9yTlQBz6J3mgAAAzpJREFUSMeNk91rHFUYh58zc2YnO7truhCxpoVUBW1siUpEKwkiiKV+VFoq3Xgjov4BVkEQDfSi/gleCH7infTCu95p8cabkNBu0xRsIwUVv+pXt5vu7M7+vJiT2d1kN7u/gTnnzPu+z5nzvu+BbpmeWQ7uOGJvev+Gh9PVKDLwbDh+L/fs250G3X/MF0Ko/AIQjISYKNk1I+pW9m24rxIJmZgY+br7+CgYA0/k89cQTU+TlUeeDoVMAyEaKNDeE6NgAog+QiTU7no3L2Ruo7FqrprOjPzKcIwFPkM0SXAIs/5AaaJk1hG3EWIoxgKfIpqINg1kro6XAfbtyv+A2EAM/RsLfOwgDVRemx0HLBbmSsXVTQwLO2FC4EtEixjlr86XM2cLM4XwUoapDOqbHPgvb9aidHmuBFh8PDz8rZjiyX5/kwN/AaUJjFafKgK2p4MtzEa5aooJtGcbJoBcBaUOhRTRVx2M1eSx7kMF4GeIsdWZwk4lnI1sFZkN5Kt8dBMTQD5DRJdmCq5OY7zDEn9R50fOcrhTwdkouJh6G0XPu0NNvGhdOsMOYhcXUM/zQQezN28vphFG/nNAeMQIEaNw5UDRIdJSC7HC59xw86MdzGRkl9MoVHiG4AYyMQnKP54laiI1s4IF9pNew7XuWgaPIRITo+h3z49BAcCGARIAHnTFO0cLuML3AOxn2hU9gaYHSAGo5ZVPBuftFbyeEkRu9N3YcuPDXZ0D4JtrwTfFhXT+OkIccmE+Uy4Ll8kBe6i59VuZBxxCyDsF4BFCEm5phut8C8A0y3zCEpuds+2+tHNA6JEA6rUAb/AbAAd4jd387Cw1Z+1IQOKxXcKwzqN8wR80WOYlzjvLr5gtGwIY27e3BfzEq9n6fTcu90EAHv11mrMs8SdTwEPMAHCB9f7OdgDkTk4AcI7vOO62Oj3AdyDkPZ7kIDDNtPuyyNeDIIOO8w9znKFKjSa/8BVznGGg7EDLfyyyyEjqhniA7xq7taO/T9J9hm5IDYiHbNrK3jXAtWkHIj7kOrmeThB1IuAWhZ6rZ4iZQpj0itqMJ+aZHy0HTm1MB22BVxB/U6NNfejTps4GNcSbafT/ojtVLdMsay4AAACEZVhJZk1NACoAAAAIAAUBEgADAAAAAQABAAABGgAFAAAAAQAAAEoBGwAFAAAAAQAAAFIBKAADAAAAAQACAACHaQAEAAAAAQAAAFoAAAAAAAAAkAAAAAEAAACQAAAAAQADoAEAAwAAAAEAAQAAoAIABAAAAAEAAAAioAMABAAAAAEAAAAiAAAAABBCQMEAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjItMTItMzBUMDA6NDc6MDIrMDA6MDClH7CTAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIyLTEyLTMwVDAwOjQ3OjAyKzAwOjAw1EIILwAAABF0RVh0ZXhpZjpDb2xvclNwYWNlADEPmwJJAAAAEnRFWHRleGlmOkV4aWZPZmZzZXQAOTBZjN6bAAAAF3RFWHRleGlmOlBpeGVsWERpbWVuc2lvbgAzNGHPwiIAAAAXdEVYdGV4aWY6UGl4ZWxZRGltZW5zaW9uADM0vFkbpwAAAABJRU5ErkJggg==",
}

CONTROLLER_HOST = os.getenv("VAR_CONTROLLER_HOST", "192.168.1.1")
CONTROLLER_USERNAME = os.getenv("VAR_CONTROLLER_USERNAME", "admin")
CONTROLLER_PASSWORD = os.getenv("VAR_CONTROLLER_PASSWORD", "admin")
CONTROLLER_PORT = int(os.getenv("VAR_CONTROLLER_PORT", "443"))
CONTROLLER_VERSION = os.getenv("VAR_CONTROLLER_VERSION", "UDMP-unifiOS")
CONTROLLER_SSL_VERIFY = os.getenv("VAR_CONTROLLER_SSL_VERIFY") == "true"

CLOUDIMAGE_TOKEN = os.getenv("VAR_CLOUDIMAGE_TOKEN")
TERMINAL_NOTIFIER_PATH = os.getenv("VAR_TERMINAL_NOTIFIER_PATH")
MENU_BAR_EVENTS = os.getenv("VAR_MENU_BAR_EVENTS") == "true"
NOTIFY_CONNECT = os.getenv("VAR_NOTIFY_CONNECT") == "true"
NOTIFY_DISCONNECT = os.getenv("VAR_NOTIFY_DISCONNECT") == "true"
NOTIFY_ROAM = os.getenv("VAR_NOTIFY_ROAM") == "true"

SHOW_TEST_DEVICES = os.getenv("VAR_SHOW_TEST_DEVICES") == "true"


def kv_to_dict(raw):
    return {
        key_value[0]: key_value[1]
        for key_value in (setting.split("=", 2) for setting in raw.split(";"))
    }


ap_aliases = os.getenv("VAR_ACCESS_POINT_ALIASES")
AP_ALIASES = kv_to_dict(ap_aliases) if ap_aliases else {}

avatars = os.getenv("VAR_AVATARS")
AVATARS = kv_to_dict(avatars) if avatars else {}


XBAR_NESTING = 0


@contextmanager
def xbar_submenu():
    global XBAR_NESTING

    XBAR_NESTING += 1
    try:
        yield
    finally:
        XBAR_NESTING -= 1


def xbar(text=None, icon=None, separator=False, nest=-1, **kwargs):
    if nest == -1:
        nest = XBAR_NESTING

    if separator:
        print("--" * nest + "---")

    segments = []

    if icon:
        if re.match(r"^[a-z_-]+$", icon):
            segments.append(f":{icon}:")
        else:
            segments.append(icon)

    if text:
        segments.append(str(text))

    for key, value in kwargs.items():
        if value is not None:
            segments.append(f"| {key}={value}")

    if segments:
        print("--" * nest + " ".join(segments))


def xbar_kv(label, value, tabs=0, **params):
    xbar("".join([label, "\t" * tabs, str(value)]), **params)


def xbar_timestamp(label, timestamp, suffix="ago", **params):
    xbar_kv(label, relative_time(timestamp, suffix), **params)

    alt_params = params.copy()
    alt_params.pop("separator", None)
    xbar_kv(label, timestamp, **alt_params, alternate=True)


def notify(title, message, image_url=None):
    if not TERMINAL_NOTIFIER_PATH:
        return

    args = []
    args.extend(["-title", title])
    args.extend(["-message", message])
    if image_url:
        args.extend(["-contentImage", image_url])

    result = subprocess.run([TERMINAL_NOTIFIER_PATH, *args])
    result.check_returncode()


# Based on https://gist.github.com/zhangsen/1199964/7225c00d65605b5fc2b106346a6f8b4bca860b6c
def relative_time(date, suffix=None):
    """Take a datetime and return its "age" as a string.
    The age can be in second, minute, hour, day, month or year. Only the
    biggest unit is considered, e.g. if it's 2 days and 3 hours, "2 days" will
    be returned.
    Make sure date is not in the future, or else it won't work.
    """

    if not date:
        return None

    def formatn(n, s):
        """Add "s" if it's plural"""

        if n == 1:
            return "1 %s" % s
        elif n > 1:
            return "%d %ss" % (n, s)

    def q_n_r(a, b):
        """Return quotient and remaining"""

        return a / b, a % b

    class PrettyDelta:
        def __init__(self, dt):
            now = datetime.now().astimezone(dt.tzinfo)
            delta = now - dt
            self.day = delta.days
            self.second = delta.seconds

            self.year, self.day = q_n_r(self.day, 365)
            self.month, self.day = q_n_r(self.day, 30)
            self.hour, self.second = q_n_r(self.second, 3600)
            self.minute, self.second = q_n_r(self.second, 60)

        def format(self):
            for period in ["year", "month", "day", "hour", "minute", "second"]:
                n = floor(getattr(self, period))
                if n == 0:
                    continue

                formatted = formatn(n, period)
                if not formatted:
                    continue
                if period == "second":
                    formatted = "less than a minute"
                if suffix:
                    formatted += " ago"
                return formatted
            return "right now"

    return PrettyDelta(date).format()


async def resize_image_content(content, size):
    with tempfile.TemporaryDirectory() as temp_dir:
        temp_dir_path = Path(temp_dir)
        original_path = temp_dir_path / "original"
        resized_path = temp_dir_path / "resized"

        original_path.write_bytes(content)

        process = await asyncio.create_subprocess_exec(
            "sips",
            original_path,
            "-Z",
            str(size * 2),
            "-s",
            "dpiHeight",
            "144.0",
            "-s",
            "dpiWidth",
            "144.0",
            "--out",
            resized_path,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        await process.wait()

        content = resized_path.read_bytes()

    return content


async def read_url(url, session):
    try:
        async with session.get(URL(url, encoded=True)) as response:
            return await response.read()
    except aiohttp.client_exceptions.ClientResponseError:
        return None


class Device:
    IPHONE_IMAGE_URL = "https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/240/apple/325/mobile-phone_1f4f1.png"
    OTHER_IMAGE_URL = "https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/240/google/350/mobile-phone_1f4f1.png"

    VENDOR_NAMES = {
        320: "iPhone",
        96: "Samsung Galaxy",
        7: "Google Pixel",
    }

    OS_NAMES = {
        24: "iPhone",
        56: "Android",
    }

    PHONE_HINTS = ["phone"]

    NAME_PATTERNS = [
        r"^(.+?)['’]s",  # "NAME's iPhone"
        r" (?:van|de) (.+)$",  # "iPhone van NAME" (Dutch), "iPhone de NAME" (Spanish)
    ]
    HOSTNAME_PATTERNS = [
        r"^(.+?)-s-",  # "NAME-s-Pixel"
        r"^(.+?)s-iPhone$",  # "NAMEs-iPhone"
        r"^iPhone(?:van|de)(.+)$",  # "iPhonevanNAME" (Dutch), "iPhonedeNAME" (Spanish)
    ]

    def __init__(self, raw):
        self.raw = raw

        self.cache = {}

        self._display_name = None

    @property
    def name(self):
        return self.raw.get("name") or None

    @property
    def hostname(self):
        return self.raw.get("hostname") or None

    @property
    def device_name(self):
        return self.name or self.hostname

    @property
    def ip(self):
        return self.raw.get("ip")

    @property
    def mac(self):
        return self.raw.get("mac")

    @property
    def vendor(self):
        return self.VENDOR_NAMES.get(self.raw.get("dev_vendor"))

    @property
    def os(self):
        return self.OS_NAMES.get(self.raw.get("os_name"))

    @property
    def is_phone(self):
        return self.raw.get("dev_family") in [9, 12] or any(
            self.device_name and hint in self.device_name.lower()
            for hint in self.PHONE_HINTS
        )

    @property
    def device_type(self):
        for vendor in self.VENDOR_NAMES.values():
            if self.device_name and vendor in self.device_name:
                return vendor

        return self.vendor or self.os

    @property
    def descriptor(self):
        descriptor = self.device_type or "phone"
        device_name = self.device_name
        if device_name:
            descriptor += f" ({device_name})"
        return descriptor

    @property
    def wifi_ssid(self):
        return self.raw.get("essid")

    @property
    def ap_mac(self):
        return self.raw.get("ap_mac")

    @property
    def is_guest(self):
        return self.raw.get("is_guest")

    @property
    def last_connected(self):
        timestamp = self.cache.get("last_connected")
        return (
            datetime.fromtimestamp(timestamp).replace(tzinfo=timezone.utc)
            if timestamp
            else None
        )

    @property
    def last_roamed(self):
        timestamp = self.cache.get("last_roamed")
        return (
            datetime.fromtimestamp(timestamp).replace(tzinfo=timezone.utc)
            if timestamp
            else None
        )

    @property
    def last_disconnected(self):
        timestamp = self.cache.get("last_disconnected")
        return (
            datetime.fromtimestamp(timestamp).replace(tzinfo=timezone.utc)
            if timestamp
            else None
        )

    @property
    def display_name(self):
        if self._display_name is not None:
            return self._display_name

        def match_pattern(value, patterns):
            if not value:
                return None

            for pattern in patterns:
                match = re.search(pattern, value)
                if match:
                    return match[1]

        self._display_name = (
            match_pattern(self.name, self.NAME_PATTERNS)
            or match_pattern(self.hostname, self.HOSTNAME_PATTERNS)
            or f"Unknown {self.descriptor}"
        )
        return self._display_name

    def __str__(self):
        return self.display_name

    @property
    def avatar_id(self):
        return AVATARS.get(self.display_name)

    @property
    def default_image_url(self):
        if self.device_type == "iPhone":
            return self.IPHONE_IMAGE_URL

        return self.OTHER_IMAGE_URL

    async def default_image(self, session):
        return await read_url(self.default_image_url, session)

    def avatar_url(self, size=32):
        avatar_id = self.avatar_id
        if not avatar_id:
            return None

        # Email
        if "@" in avatar_id:
            email_hash = hashlib.md5(avatar_id.encode("utf-8")).hexdigest()
            return f"https://www.gravatar.com/avatar/{email_hash}?s={size * 3}&d=404"

        # URL
        return avatar_id

    async def avatar(self, session, size=32):
        avatar_url = self.avatar_url(size=size)
        if not avatar_url:
            return None

        content = await read_url(avatar_url, session)
        if not content:
            return None

        if CLOUDIMAGE_TOKEN:
            original_url = URL(avatar_url)
            avatar_url = f"https://{CLOUDIMAGE_TOKEN}.cloudimg.io/{original_url.host}{original_url.path}?width={size * 2}&height={size * 2}&radius=1000&force_format=png"
            content = await read_url(avatar_url, session)

        return content

    def image_url(self, size=32):
        return self.avatar_url(size=size) or self.default_image_url

    async def image(self, session, size=32):
        content = await self.avatar(session, size=size) or await self.default_image(
            session
        )
        if not content:
            return None

        if size:
            content = await resize_image_content(content, size)

        return content

    async def image_b64(self, session, size=32):
        content = await self.image(session, size=size)
        if not content:
            return None

        return base64.b64encode(content).decode()

    @property
    def ap_name(self):
        return self.cache.get("ap_name", self.ap_mac)

    @property
    def previous_ap_name(self):
        return self.cache.get("previous_ap_name")

    @property
    def event(self):
        previous_ap = self.previous_ap_name
        current_ap = self.ap_name

        if current_ap == previous_ap:
            return (
                None,
                [
                    f"has been in {current_ap}",
                    *(
                        [
                            f"for {relative_time(self.last_roamed) or relative_time(self.last_connected)}"
                        ]
                        if self.last_roamed or self.last_connected
                        else []
                    ),
                ],
            )

        if not current_ap:
            return (
                "disconnect",
                [
                    "left",
                    *(
                        [f"after {relative_time(self.last_connected)}"]
                        if self.last_connected
                        else []
                    ),
                ],
            )

        if not previous_ap:
            return (
                "connect",
                [
                    "arrived",
                    f"via {current_ap}",
                    *(
                        [f"after {relative_time(self.last_disconnected)} away"]
                        if self.last_disconnected
                        else []
                    ),
                ],
            )

        return (
            "roam",
            [
                "moved",
                f"from {previous_ap} to {current_ap}",
                *(
                    [
                        f"after {relative_time(self.last_roamed) or relative_time(self.last_connected)}"
                    ]
                    if self.last_roamed or self.last_connected
                    else []
                ),
            ],
        )


class FakeDevice(Device):
    def __init__(self, display_name, ap_name=None):
        super().__init__({})

        self._display_name = display_name
        self._ap_name = ap_name

    @property
    def display_name(self):
        return self._display_name

    @property
    def name(self):
        return self._display_name

    @property
    def hostname(self):
        return self._display_name

    @property
    def ap_mac(self, *_):
        return self._ap_name


class WhosHomeApp:
    CACHE_PATH = Path(".whos-home-unifi/cache.json")

    def __init__(self):
        self._controller = None
        self._devices = None
        self._ap_names = None

    @property
    def unifi_controller_url(self):
        https = CONTROLLER_PORT == 443
        return f"http{'s' if https else ''}://{CONTROLLER_HOST}:{CONTROLLER_PORT}"

    @property
    def controller(self):
        if self._controller is not None:
            return self._controller

        self._controller = Controller(
            CONTROLLER_HOST,
            username=CONTROLLER_USERNAME,
            password=CONTROLLER_PASSWORD,
            port=CONTROLLER_PORT,
            version=CONTROLLER_VERSION,
            ssl_verify=CONTROLLER_SSL_VERIFY,
        )
        return self._controller

    @property
    def devices(self):
        if self._devices is not None:
            return self._devices

        if SHOW_TEST_DEVICES:
            devices = [
                FakeDevice(name, ap_name=random.choice(list(self.ap_names.values())))
                for name in [
                    *AVATARS.keys(),
                    "iPhone",
                    "Android",
                ]
            ]
        else:
            devices = [
                device
                for device in (
                    Device(client) for client in self.controller.get_clients()
                )
                if device.is_phone and device.ap_mac
            ]

        devices.sort(
            key=lambda device: (
                not device.avatar_id,  # Devices with avatars first
                device.display_name,  # Then sorted by name
            ),
        )

        self._devices = devices
        return self._devices

    @property
    def ap_names(self):
        if self._ap_names is not None:
            return self._ap_names

        if SHOW_TEST_DEVICES:
            return {name: name for name in (f"Laboratory {i}" for i in range(1, 5))}

        self._ap_names = {
            ap["mac"]: AP_ALIASES.get(ap["name"])
            or AP_ALIASES.get(ap["mac"])
            or ap["name"]
            for ap in self.controller.get_aps()
            if ap.get("is_access_point", False)
        }
        return self._ap_names

    @property
    def devices_by_ap_name(self):
        ap_devices = defaultdict(list)
        for device in self.devices:
            ap_devices[device.ap_name].append(device)
        return ap_devices

    def changed_devices(self):
        ts = int(datetime.utcnow().timestamp())
        cache = {}

        self.CACHE_PATH.parent.mkdir(parents=True, exist_ok=True)

        try:
            previous_cache = json.loads(self.CACHE_PATH.read_text())
        except FileNotFoundError:
            previous_cache = {}

        devices_by_name = {device.display_name: device for device in self.devices}
        all_device_names = previous_cache.keys() | devices_by_name.keys()

        changed_devices = []
        for device_name in all_device_names:
            device_cache = previous_cache.get(device_name, {})
            cache[device_name] = device_cache

            device = devices_by_name.get(device_name) or FakeDevice(device_name)
            device.cache = device_cache

            # Sets device.ap_name and device.previous_ap_name through device_cache
            device_cache["previous_ap_name"] = device_cache.get("ap_name")
            device_cache["ap_name"] = self.ap_names.get(device.ap_mac, device.ap_mac)

            change_type, _ = device.event

            if not change_type:
                continue

            if change_type == "disconnect":
                device_cache["last_disconnected"] = ts
                if not NOTIFY_DISCONNECT:
                    continue
            elif change_type == "connect":
                device_cache["last_connected"] = ts
                if not NOTIFY_CONNECT:
                    continue
            elif change_type == "roam":
                device_cache["last_roamed"] = ts
                if not NOTIFY_ROAM:
                    continue

            changed_devices.append(device)

        self.CACHE_PATH.write_text(json.dumps(cache))

        return changed_devices

    async def xbar_device(self, device, session, **params):
        image = await device.image_b64(session, size=26)

        xbar(device.display_name, refresh=True, size=14, image=image, **params)

        with xbar_submenu():
            _, event_segments = device.event
            xbar(
                " ".join([device.display_name, *event_segments]),
                refresh=True,
                image=image,
            )

            xbar_kv("Name:", device.name, tabs=4, separator=True)
            xbar_kv("Hostname:", device.hostname or "N/A", tabs=3)
            xbar_kv("MAC:", device.mac, tabs=4, alternate=True)
            xbar_kv("IP:", device.ip, tabs=5)

            xbar_kv("WiFi:", device.wifi_ssid, tabs=4, separator=True)
            xbar_kv("AP MAC:", device.ap_mac, tabs=4, alternate=True)
            xbar_kv("Guest:", "Yes" if device.is_guest else "No", tabs=4)

            xbar_timestamp(
                "Last connected:", device.last_connected, tabs=2, separator=True
            )
            xbar_timestamp("Last roamed:", device.last_roamed, tabs=3)
            xbar_timestamp("Last disconnected:", device.last_disconnected, tabs=1)

            if device.raw:
                xbar("Raw", separator=True)

                with xbar_submenu():
                    for key, value in device.raw.items():
                        if isinstance(value, int) and re.match(
                            r"^[0-9]{10}$", str(value)
                        ):
                            value = datetime.fromtimestamp(value).astimezone()
                            xbar_timestamp(f"{key} = ", value)
                        else:
                            xbar_kv(f"{key} = ", value)

    def xbar_icon(self, device_count=None):
        xbar(templateImage=MENUBAR_NUMBER_ICONS_B64.get(device_count, MENUBAR_ICON_B64))

    def xbar_refresh(self, **params):
        xbar("Refresh", refresh=True, **params)

    def xbar_unifi_controller(self, **params):
        xbar("Open UniFi Controller...", href=self.unifi_controller_url, **params)

    def xbar_error(self, message, err=None, **params):
        xbar(message, color="red", icon="warning", **params)
        if err:
            print(err)


async def main():
    app = WhosHomeApp()

    try:
        devices = app.devices
    except APIError as err:
        app.xbar_icon()

        app.xbar_error(
            "Failed to connect to UniFi Controller",
            err,
            separator=True,
        )
        app.xbar_unifi_controller()
        app.xbar_refresh()

        return

    async with aiohttp.ClientSession(raise_for_status=True) as session:
        changed_devices = app.changed_devices()
        if changed_devices:
            for device in changed_devices:
                _, [event_verb, *event_context] = device.event
                title = " ".join([device.display_name, event_verb])

                if MENU_BAR_EVENTS:
                    xbar(
                        f" {title}",
                        trim=False,
                        image=await device.image_b64(session, size=17),
                    )

                notify(
                    title=title,
                    message=" ".join(event_context),
                    image_url=device.image_url(size=64),
                )

        if not (changed_devices and MENU_BAR_EVENTS):
            app.xbar_icon(len(devices))

        if devices:
            for ap, devices in app.devices_by_ap_name.items():
                xbar(ap, separator=True)

                for device in devices:
                    await app.xbar_device(device, session)

                if len(devices) > 5:
                    xbar(f"{len(devices)} people", size=11)

            xbar(separator=True)
        else:
            xbar("No one's home", separator=True)

    app.xbar_refresh()
    app.xbar_unifi_controller(alternate=True)


if __name__ == "__main__":
    asyncio.run(main())
