#!/usr/bin/python3
# <xbar.title>ZeroTier Status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Holger Hoffmann</xbar.author>
# <xbar.author.github>the-holger</xbar.author.github>
# <xbar.desc>View and manage ZeroTier Network connections</xbar.desc>
# <xbar.image>https://github.com/the-holger/zerotier-status/blob/main/zerotier-status.png?raw=true</xbar.image>
# <xbar.dependencies>python3,zerotier-cli</xbar.dependencies>
# <xbar.var>string(SAVED_NETWORK_IDS=""): 16-digit ZeroTier network_ids [Comma separated]</xbar.var>

# You will need to add the following line to your sudoers file. Remember to edit
# sudoers with `sudo visudo`.
#
# %admin          ALL = NOPASSWD:/usr/local/bin/zerotier-cli*
import subprocess
import json
import os
import sys
import re

SUDO_EXECUTABLE = "/usr/bin/sudo"
ZEROTIER_EXECUTABLE = "/usr/local/bin/zerotier-cli"

MENUBAR_ICON_CONNECTED="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAEsmlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPD94cGFja2V0IGJlZ2luPSLvu78iIGlkPSJXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQiPz4KPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNS41LjAiPgogPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgeG1sbnM6ZXhpZj0iaHR0cDovL25zLmFkb2JlLmNvbS9leGlmLzEuMC8iCiAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyIKICAgIHhtbG5zOnBob3Rvc2hvcD0iaHR0cDovL25zLmFkb2JlLmNvbS9waG90b3Nob3AvMS4wLyIKICAgIHhtbG5zOnhtcD0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wLyIKICAgIHhtbG5zOnhtcE1NPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvbW0vIgogICAgeG1sbnM6c3RFdnQ9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZUV2ZW50IyIKICAgZXhpZjpQaXhlbFhEaW1lbnNpb249IjMyIgogICBleGlmOlBpeGVsWURpbWVuc2lvbj0iMzIiCiAgIGV4aWY6Q29sb3JTcGFjZT0iMSIKICAgdGlmZjpJbWFnZVdpZHRoPSIzMiIKICAgdGlmZjpJbWFnZUxlbmd0aD0iMzIiCiAgIHRpZmY6UmVzb2x1dGlvblVuaXQ9IjIiCiAgIHRpZmY6WFJlc29sdXRpb249IjE0NC8xIgogICB0aWZmOllSZXNvbHV0aW9uPSIxNDQvMSIKICAgcGhvdG9zaG9wOkNvbG9yTW9kZT0iMyIKICAgcGhvdG9zaG9wOklDQ1Byb2ZpbGU9InNSR0IgSUVDNjE5NjYtMi4xIgogICB4bXA6TW9kaWZ5RGF0ZT0iMjAyMy0wMy0xNVQwODoxMTo0OCswMTowMCIKICAgeG1wOk1ldGFkYXRhRGF0ZT0iMjAyMy0wMy0xNVQwODoxMTo0OCswMTowMCI+CiAgIDx4bXBNTTpIaXN0b3J5PgogICAgPHJkZjpTZXE+CiAgICAgPHJkZjpsaQogICAgICBzdEV2dDphY3Rpb249InByb2R1Y2VkIgogICAgICBzdEV2dDpzb2Z0d2FyZUFnZW50PSJBZmZpbml0eSBQaG90byAxLjEwLjYiCiAgICAgIHN0RXZ0OndoZW49IjIwMjMtMDMtMTVUMDg6MTE6NDgrMDE6MDAiLz4KICAgIDwvcmRmOlNlcT4KICAgPC94bXBNTTpIaXN0b3J5PgogIDwvcmRmOkRlc2NyaXB0aW9uPgogPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KPD94cGFja2V0IGVuZD0iciI/PuS5jtYAAAGAaUNDUHNSR0IgSUVDNjE5NjYtMi4xAAAokXWR3yuDURjHPzaa/GiK4sLF0rgyDbW4USaNWtJMGW62d7/UNm/v+0pyq9yuKHHj1wV/AbfKtVJESm65Jm5Yr+fdVluy5/Sc53O+5zxP5zwHbOGMktXrvZDNGVoo4HctRBZdjlfsOHEAnVFFV8dnZ4PUtK8H6qx457Fq1T73rzXHE7oCdY3CY4qqGcJTwsF1Q7V4V7hDSUfjwufC/ZpcUPje0mMlfrM4VeIfi7VwaAJsbcKuVBXHqlhJa1lheTnubGZNKd/HeklLIjc/J7FHvBudEAH8uJhmkgl8DDIqsw8PQwzIihr53mL+DKuSq8issoHGCinSGPSLuibVExKToidkZNiw+v+3r3pyeKhUvcUPDS+m+dELjh0o5E3z+9g0Cydgf4arXCV/9QhGPkXPVzT3ITi34OK6osX24HIbup7UqBYtSnZxWzIJ72fQGoH2W2haKvWsvM/pI4Q35atuYP8A+uS8c/kX45Bnql7MO4EAAAAJcEhZcwAAFiUAABYlAUlSJPAAAAGASURBVFiF7ZdNTgJBEIW/Nl5gElyjZ4C9RjcuERK4AeNJ9CxwAjcaXRJmTsFSjAvcjwu6052hf6aZNhMTXkImqapUvel5XVUIoKJDnHVZPAUqWp5g5ydwInAeGZ8BN8AQGBj2F6AECuAd+E7A7QAz4AstPNdvK2OT4QJYGgVWwBMwNmxjaVsZtiXQa1u8B2xkwh0wB4Thr19DAeTAj7Rv2pJQb74GLi1+Vx+4Yq+HClgcW3yGfnNbcR8BRWIn/dPY4hlacHNPXKgT5mhhZjEEHtCCE564EAGBFubIFuBqREP5fA0UCKEC3mo5owgULYorqBwDb1QNW/bs+4G4JtOwL2M+bc7OZ4GLQCmfUcfmgPdzugioYKtwjiRQ2pwhAnf4r2EIArit5WyEDC3EThoRpGnFaihFt2IFNYwKmbApgSTDCA7HcU54HD+ScBwrEvWF5BmYGLaJtJkLySJFcRNTtDBDK1njbx57xTLgGr2U3ku7uZR+8EdLqQ2nf0b/n8AvYFiMPyrV38QAAAAASUVORK5CYII="
MENUBAR_ICON_DISCONNECTED="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAQAAADZc7J/AAAEsmlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPD94cGFja2V0IGJlZ2luPSLvu78iIGlkPSJXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQiPz4KPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNS41LjAiPgogPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgeG1sbnM6ZXhpZj0iaHR0cDovL25zLmFkb2JlLmNvbS9leGlmLzEuMC8iCiAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyIKICAgIHhtbG5zOnBob3Rvc2hvcD0iaHR0cDovL25zLmFkb2JlLmNvbS9waG90b3Nob3AvMS4wLyIKICAgIHhtbG5zOnhtcD0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wLyIKICAgIHhtbG5zOnhtcE1NPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvbW0vIgogICAgeG1sbnM6c3RFdnQ9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZUV2ZW50IyIKICAgZXhpZjpQaXhlbFhEaW1lbnNpb249IjMyIgogICBleGlmOlBpeGVsWURpbWVuc2lvbj0iMzIiCiAgIGV4aWY6Q29sb3JTcGFjZT0iNjU1MzUiCiAgIHRpZmY6SW1hZ2VXaWR0aD0iMzIiCiAgIHRpZmY6SW1hZ2VMZW5ndGg9IjMyIgogICB0aWZmOlJlc29sdXRpb25Vbml0PSIyIgogICB0aWZmOlhSZXNvbHV0aW9uPSIxNDQvMSIKICAgdGlmZjpZUmVzb2x1dGlvbj0iMTQ0LzEiCiAgIHBob3Rvc2hvcDpDb2xvck1vZGU9IjEiCiAgIHBob3Rvc2hvcDpJQ0NQcm9maWxlPSJHcmV5c2NhbGUgRDUwIgogICB4bXA6TW9kaWZ5RGF0ZT0iMjAyMy0wMy0xNVQwODo1MDo0NCswMTowMCIKICAgeG1wOk1ldGFkYXRhRGF0ZT0iMjAyMy0wMy0xNVQwODo1MDo0NCswMTowMCI+CiAgIDx4bXBNTTpIaXN0b3J5PgogICAgPHJkZjpTZXE+CiAgICAgPHJkZjpsaQogICAgICBzdEV2dDphY3Rpb249InByb2R1Y2VkIgogICAgICBzdEV2dDpzb2Z0d2FyZUFnZW50PSJBZmZpbml0eSBQaG90byAxLjEwLjYiCiAgICAgIHN0RXZ0OndoZW49IjIwMjMtMDMtMTVUMDg6NTA6NDQrMDE6MDAiLz4KICAgIDwvcmRmOlNlcT4KICAgPC94bXBNTTpIaXN0b3J5PgogIDwvcmRmOkRlc2NyaXB0aW9uPgogPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KPD94cGFja2V0IGVuZD0iciI/PtPoylcAAADdaUNDUEdyZXlzY2FsZSBENTAAABiVY2BgjMlJzi1mMWBgyM0rKXIPcoyMiIxSYH/OwMzAz8DOwMAgnphcXOAYEODDgBN8u8bACKIv64LMwq0OK2BJSS1OBtJbgNgsuaCoBEi/AWKf8pICIJvRAsgWyQ4JcgayQW4QyM0pTYbqBdnKk5oXGgykpYBYhsGdoYghlaGSoZghmSGRIQfIVmBwYTBlAPkPuz4DsD4/hnygymQgWQDUXcSQyZDOkMFQwqADFC0FmgYyJw1sdirQ1EpQGKH7vSCxKBHuKyZjYwBkEjJv302flwAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAZdJREFUSInN1cFqU1EUheGvpjEphFAU0wYirYNbKJiBFBEKFdKBIHkD6cPYdymCM0HUWQP1BYQKStMOajNIMrMUm5gKDprBvddzEzGIruE+e/2sfQ7sM+e5mXRjNvt07dr9ywn+PSCXUS9YtW5DU9WlW2765keocT5oX9NQBHmsWMFAy9HvJFjwxCPzuj75oKrrpa9yFkVu67ianGDBMyUjBz6C7zh2jLotkaoXLuOG9CU2lPTsje1xHdrTV9JIlpOANZGRt85/nRXn3hiJRFmAggYOMuzXiPfYVggDaoq6gfBxHeoqqoUBSzibaIcOKtmA/lRAb9wZAFTGx9MBGQn+SHFAXyJchlKDxgGpcBMAsUHTgLtTAbXsBB0Dy+5PtNctG+iEAUMtPFbOtJdtYd8wDOBIW14zA1HWlNfWjhfTz9hyoWJHPRB+R8WFVrKcXihXPiu7455Viwoeyjm1blNdTtvr5DZgLhg2sj1eaU/xDgzsJ8NfK7wT276oqVgywqmevk786qYBGDpxggd4ldGD/+FjmRnwE2SsXnN8yAvoAAAAAElFTkSuQmCC"
MENUBAR_ICON_WARN="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAF4GlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPD94cGFja2V0IGJlZ2luPSLvu78iIGlkPSJXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQiPz4KPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNS41LjAiPgogPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgeG1sbnM6ZXhpZj0iaHR0cDovL25zLmFkb2JlLmNvbS9leGlmLzEuMC8iCiAgICB4bWxuczpwaG90b3Nob3A9Imh0dHA6Ly9ucy5hZG9iZS5jb20vcGhvdG9zaG9wLzEuMC8iCiAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyIKICAgIHhtbG5zOnhtcD0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wLyIKICAgIHhtbG5zOnhtcE1NPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvbW0vIgogICAgeG1sbnM6c3RFdnQ9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZUV2ZW50IyIKICAgIHhtbG5zOmRjPSJodHRwOi8vcHVybC5vcmcvZGMvZWxlbWVudHMvMS4xLyIKICAgZXhpZjpDb2xvclNwYWNlPSIxIgogICBleGlmOlBpeGVsWERpbWVuc2lvbj0iMzIiCiAgIGV4aWY6UGl4ZWxZRGltZW5zaW9uPSIzMiIKICAgcGhvdG9zaG9wOkNvbG9yTW9kZT0iMyIKICAgcGhvdG9zaG9wOklDQ1Byb2ZpbGU9InNSR0IgSUVDNjE5NjYtMi4xIgogICB0aWZmOkltYWdlTGVuZ3RoPSIzMiIKICAgdGlmZjpJbWFnZVdpZHRoPSIzMiIKICAgdGlmZjpSZXNvbHV0aW9uVW5pdD0iMiIKICAgdGlmZjpYUmVzb2x1dGlvbj0iMTQ0LzEiCiAgIHRpZmY6WVJlc29sdXRpb249IjE0NC8xIgogICB4bXA6TWV0YWRhdGFEYXRlPSIyMDIzLTAzLTE1VDEwOjA2OjA0KzAxOjAwIgogICB4bXA6TW9kaWZ5RGF0ZT0iMjAyMy0wMy0xNVQxMDowNjowNCswMTowMCI+CiAgIDx4bXBNTTpIaXN0b3J5PgogICAgPHJkZjpTZXE+CiAgICAgPHJkZjpsaQogICAgICB4bXBNTTphY3Rpb249InByb2R1Y2VkIgogICAgICB4bXBNTTpzb2Z0d2FyZUFnZW50PSJBZmZpbml0eSBQaG90byAxLjEwLjYiCiAgICAgIHhtcE1NOndoZW49IjIwMjMtMDMtMTVUMDg6NTA6NDQrMDE6MDAiLz4KICAgICA8cmRmOmxpCiAgICAgIHN0RXZ0OmFjdGlvbj0icHJvZHVjZWQiCiAgICAgIHN0RXZ0OnNvZnR3YXJlQWdlbnQ9IkFmZmluaXR5IFBob3RvIDEuMTAuNiIKICAgICAgc3RFdnQ6d2hlbj0iMjAyMy0wMy0xNVQxMDowNjowNCswMTowMCIvPgogICAgPC9yZGY6U2VxPgogICA8L3htcE1NOkhpc3Rvcnk+CiAgIDxkYzp0aXRsZT4KICAgIDxyZGY6QWx0PgogICAgIDxyZGY6bGkgeG1sOmxhbmc9IngtZGVmYXVsdCI+WlRfVEhJTl9XQVJOPC9yZGY6bGk+CiAgICA8L3JkZjpBbHQ+CiAgIDwvZGM6dGl0bGU+CiAgPC9yZGY6RGVzY3JpcHRpb24+CiA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgo8P3hwYWNrZXQgZW5kPSJyIj8+3Us5YwAAAYFpQ0NQc1JHQiBJRUM2MTk2Ni0yLjEAACiRdZHfK4NRGMc/22h+TFMkFy6WxtWmmVrcKFtCSZopw8327pfa5u19Jy23yq2ixI1fF/wF3CrXShEpueWauEGv57XVluw5ned8zvc8z9M5zwFrJKvk9Dof5PIFLTwWdM1F5132Z2w4acRCR0zR1ZHp6Ulq2sedxIndeM1ateP+teZEUlfA0iA8rKhaQXhceHK1oJq8LdyuZGIJ4VNhjyYXFL419XiJX0xOl/jLZC0SDoG1VdiVruJ4FSsZLScsL8edy64o5fuYL3Ek87MzsnbL7EInzBhBXEwwSogA/QyJD+DFT5/sqJHv+82fYllyFfEqRTSWSJOhgEfUFamelDUlelJGlqLZ/7991VMD/lJ1RxDqnwzjrQfsW/C9aRifh4bxfQS2R7jIV/KXD2DwXfTNiubeB+c6nF1WtPgOnG9A54Ma02K/kk2mNZWC1xNoiULbNTQtlHpWPuf4HiJr8lVXsLsHvRLvXPwB5/xnrB+ak2AAAAAJcEhZcwAAFiUAABYlAUlSJPAAAAJqSURBVFiF7ddNiI1RHMfxz1xjXjTezUVGSKOUsfASm1EjKVFSSml2pCSLYWcxRikLchdWFhZKomxRNsTCTjJKYYNZzL1ZDMnbjLI45zJNc+99zszNbPzqdG7POc///z2/c57+5zZgwAwqN5PJ66FzsU1ZM+7Af4BZifObsRrrsRl7sRzfsAhN+IpfWQM2JiRfhx60jHs2O/arYoPveIjXWYJmcaAVu7FNAB7GKzwXVj+M2/gU4y1AJxZjCGPVgtdyoBWH0YZRPMbLceM/Y/82NuhCd4RYjpvCFk2qWoewJyYv4saE5JU0GOeW4rs91SZXA1gnrGIU9/A5Q/KyPuNufLcztiSAZn/JHycmHw/xJP7eGWNmBugQTvuwbLZX0mCM0RJjZgZYGvsP00he1lDs81MBKNUBoDghZiaAMm2xwvhUAJIc+GeqBFC2flLbElV1OysBVLUtq0oFuadnXGjMaVBhO2sBrJwOAHrXtjs4sN8WiQ4MCVVtGTZMJXOpYO7YL5egd7t9Vw4bSQH4IZRU2IF5qQBfvhtonKUd5jSZf2irUykAhHr+Rqj5e1Mg7hy3cU6TkxMeny4VrEkBILjwRTiMvUKpraWupfNcz+X+XFbKasbFiZMbMgRsFQpTuaINC2ekiCPx2TXhc+s4tkP3+QNOVInXk+/zKAWgrE6hqo2/ku2J/X1omS33rN/pJW1WVInzApvyfeHemHInfIP3QlXLCysejWPvUHzQZ1eN5LARR3GVNAcmU/lf0dlSwcIIuTjDex/Rme8zUs9a0J8xOSyJ8+vjQKnglrC3KVs6hq56OXA5Mbk4//Jv3e93kMw3gWUAAAAASUVORK5CYII="

def load_network_ids_from_file() -> list[str]:
    """ Load previously encountered ZeroTier NetworkIDs from the plugin VAR storage (zerotierstatus.*.py.vars.json)

    This function loads previously active (or manually added) NetworkIDs and returns them as a list of strings.

    Note:
        The output is sanitized by removing NetworkIDs that do not match the ZeroTier NetworkID format (16 hex digits)

    Returns:
        list[str]: All known ZeroTier NetworkIDs as strings in a list.
    """
    # xbar stores the variables in a file called <pluginname>.vars.json, here zerotierstatus.<time>.py.vars.json
    vars_filename = f"{os.path.abspath(__file__)}.vars.json"
    saved_network_ids = []
    with open(vars_filename) as vars_file:
        vars = json.load(vars_file)
    # xbar does not allow lists/arrays to be stored in a variable, hence we use a comma-separated string
    vars_records = vars.get('SAVED_NETWORK_IDS', "").split(',')
    for network in vars_records:
        # Removing accidental whitespaces when editing the json file by hand
        network = network.strip()
        # Checking if the found ID is indeed a 16-digit hex number
        if re.match('[a-fA-F0-9]{16}', network):
            saved_network_ids.append(network)
    # Sorting the list to have at least some continuity
    saved_network_ids.sort()
    return saved_network_ids

def save_network_ids_to_file(networks: list[str]):
    """ Save a list of ZeroTier NetworkIDs to the plugin VAR storage (zerotierstatus.*.py.vars.json)
    
    This function checks a list of NetworkIDs to remove duplicates and saves them in the xbar plugin storage.

    Args:
        networks (list[str]): The NetworkIDs to be saved as a list of strings
    """
    # Casting the passed list into a set and back to remove duplicates
    networks = list(set(networks))
    networks.sort()
    # xbar stores the variables in a file called <pluginname>.vars.json, here zerotierstatus.<time>.py.vars.json
    vars_filename = f"{os.path.abspath(__file__)}.vars.json"
    vars = {"SAVED_NETWORK_IDS": f"{','.join(networks)}"}
    with open(vars_filename, "w") as vars_file:
        json.dump(vars, vars_file)
    

def add_network():
    """ Interactively add a ZeroTier NetworkID to the list of known networks (and saving it)

    This function opens a prompt to let the user enter a ZeroTier NetworkID and checks if the entered string is a 16-digit hex number.
    If the check passes the number is added to the known networks and saved in the plugin VAR storage, if it fails an error message is displayed.

    Note:
        This function exits the script after execution, there is no output for xbar to parse.
    """
    script = "/usr/bin/osascript -e 'set T to text returned of (display dialog \"16-digit Zerotier network_id:\" buttons {\"Cancel\", \"Add\"} default button \"Add\" default answer \"\")'"
    new_network_id = subprocess.Popen([script], stdout=subprocess.PIPE, shell=True).communicate()[0].strip().decode('utf-8')
    if re.match('[a-fA-F0-9]{16}', new_network_id):
        saved_network_ids = load_network_ids_from_file()
        saved_network_ids.append(new_network_id)
        save_network_ids_to_file(saved_network_ids)
    else:
        script = "/usr/bin/osascript -e 'display dialog \"This does not appear to be a 16-digit Zerotier network_id.\" buttons {\"Ok\"}'"
        subprocess.Popen([script], stdout=subprocess.PIPE, shell=True)
    exit()

def forget_network(network_id):
    """ Remove a ZeroTier NetworkID from the known networks

    This function checks if the NetworkID passed as the argument is in the plugin's VAR storage and removes it if it is.

    Args:
        network_id (str): The 16-digit hex NetworkID to remove from the list of known networks.

    Note:
        This function exits the script after execution, there is no output for xbar to parse.
    """
    saved_network_ids = load_network_ids_from_file()
    if network_id in saved_network_ids:
        saved_network_ids.remove(network_id)
        save_network_ids_to_file(saved_network_ids)
    exit()


def get_active_network_info():
    """ Gets the currently active (not only connected) networks' information from zerotier-cli

    This functions gets the currently active networks from zerotier-cli and returns as an array of JSON objects.
    In case of an error an empty array is returned.

    Note:
        Be sure to add the user you are running xbar (and hence this plugin) as to the sudoers list as mentioned above.
        Use `visudo` only to edit the sudoers file, do not change the sudoers file directly in any other text editor.
        
        If your user is in the admin group (probably is), this line should be used:
        %admin          ALL = NOPASSWD:/usr/local/bin/zerotier-cli*

        If you would like to limit access to one user only (or the user is not in the admin group) replace %admin with the username.

    Returns:
        list[str]: JSON array of objects containing the network info as JSON objects, may be an empty array.
    
    """
    process = subprocess.Popen(f"{SUDO_EXECUTABLE} {ZEROTIER_EXECUTABLE} listnetworks -j", shell=True, stdout=subprocess.PIPE)
    process.wait()
    data, _ = process.communicate()
    if process.returncode == 0:
        return json.loads(data.decode('utf-8'))
    else:
        return []

def print_active_network_info(network):
    """ Prepare the Menubar List Item for currently active networks.

    This function takes a currently active (connected, connecting or in error state) network and writes out details for xbar to parse.

    Args:
        network (dict): The JSON object network information, represented as a dict
    """
    # Menubar Item Entry: Network ID and Name
    print(f"{network['id'].upper()}\t\t{network['name']} ", end="")
    # Determining the color of the Menubar Item Entry based on the network status,
    # also setting the status_color variable to the ANSI color used in the submenu
    status_string = network['status'].lower()
    if status_string == 'ok':
        print("| color=green")
        status_color="\u001b[32m"
    elif status_string == 'requesting_configuration':
        print("| color=orange")
        status_color="\u001b[33m"
    #elif status_string in ['port_error', 'not_found', 'access_denied']:
    else:
        print("| color=red")
        status_color="\u001b[31m"

    # Some general details of the network, go into a submenu of the network menu item
    print(f"-- Ethernet\t\t{network['mac']}")
    print(f"-- Device\t\t{network['portDeviceName']}")
    print(f"-- Type\t\t{network['type']}")

    # Print the network status, color the status value according to the status_color set above
    print(f"-- Status\t\t{status_color}{network['status']}")
    
    # Networking details: IPs, Routes, Bridging and DNS
    if len(network['assignedAddresses']) > 0:
        print("-- Addresses")
        # List all the assigned addresses in a sub-submenu
        for address in network['assignedAddresses']:
            print(f"---- {address}")
    if len(network['routes']) > 0:            
        print("-- Routes")
        for route in network['routes']:
            # List all the defined routes in a sub-submenu
            print(f"---- {route['target']} via {route['via']}")
    print(f"-- Bridging\t\t{network['bridge']}")
    if len(network['dns']['domain']) > 0 or len(network['dns']['servers']) > 0:
        print(f"-- DNS")
        if len(network['dns']['domain']) > 0 :
            print(f"---- Domain\t\t{network['dns']['domain']}")
        if len(network['dns']['servers']) > 0:
            print(f"---- Servers")
            for server in network['dns']['servers']:
                print(f"------ {server}")
    
    # Submenu divider and action item to disconnect the active network, calling zerotier-cli
    print("-----")
    print(f"-- Disconnect | shell='{SUDO_EXECUTABLE}' param1='{ZEROTIER_EXECUTABLE}' param2='leave' param3='{network['id']}' terminal=false refresh=true")
    
def print_inactive_network_info(network_id):
    """ Prepare the Menubar List Item for previously active or saved networks.

    This function takes a NetworkID that was previously active (connected, connecting or in error state) or saved by the user and
    writes out details for xbar to parse.

    Args:
        network_id (str): The NetworkID, a 16-digit hex number
    """
    # We only save the NetworkID as all the other details could have changed in the backend. So not much too display here except the NetworkID
    print(f"{network_id.upper()}")
    # Action items in the submenu to (re)connect to the network (using zerotier-cli) or remove the NetworkID from the plugin's VAR storage (using this script)
    print(f"-- Reconnect | shell='{SUDO_EXECUTABLE}' param1='{ZEROTIER_EXECUTABLE}' param2='join' param3='{network_id}' terminal=false refresh=true")
    print(f"-- Forget | shell='{os.path.abspath(__file__)}' param1='forget' param2='{network_id}' terminal=false refresh=true")



if __name__ == "__main__":
    # Step 1: check if we want to execute an action using this script.
    # Currently this may be to add or remove a NetworkID from the plugin VAR storage
    # Script will exit after the action and not print anything.
    script_args = sys.argv[1:]
    if len(script_args) == 1 and script_args[0].lower() == 'add':
            add_network()
    if len(script_args) == 2 and script_args[0].lower() == 'forget':
            network_id = script_args[1].lower()
            forget_network(network_id)

    # Step 2: Retrieve all network information. This means both inactive networks from 
    # plugin VAR storage as well as active ones from zerotier-cli
    saved_network_ids = load_network_ids_from_file()
    active_networks = get_active_network_info()
    active_network_ids = [network['id'].lower() for network in active_networks]
    # If there is a active_network_id we don't have in saved_network_ids: save it!
    all_network_ids = set(saved_network_ids).union(active_network_ids)
    if set(saved_network_ids) != all_network_ids:
        save_network_ids_to_file(list(all_network_ids))
    
    # Step 3: Determine the overall status of the ZeroTier networks and choose a menubar icon accordingly
    # Highest priority: Warning icon if there's a network with a "problem" status
    # Otherwise display a solid icon if at least one network is successfully connected or a translucent one 
    # if all networks are disconnected
    if any(network['status'].lower() in ['port_error', 'not_found', 'access_denied'] for network in active_networks):
        print(f"| templateImage={MENUBAR_ICON_WARN}\n---")
    elif any(network['status'].lower() == "ok" for network in active_networks):
        print(f"| templateImage={MENUBAR_ICON_CONNECTED}\n---")
    else:
        print(f"| templateImage={MENUBAR_ICON_DISCONNECTED}\n---")

    # First create the menubar list items for active networks (connected, connecting and in an error state)...
    for network in active_networks:
        print_active_network_info(network)

    # ... then print out a list of inactive networks (i.e. loaded from plugin VAR storage)
    for network_id in saved_network_ids:
        if network_id not in active_network_ids:
            print_inactive_network_info(network_id)

    # Action item in the menubar list: adding a new NetworkID to the known networks using this script.
    print("---")
    print(f"Add ZeroTier NetworkID | shell='{os.path.abspath(__file__)}' param1='add' terminal=false refresh=true")