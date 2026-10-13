#!/usr/bin/env python3
# <xbar.title>ClickUp My Tasks</xbar.title>
# <xbar.version>v0.1.0</xbar.version>
# <xbar.author>Alexander Wansiedler</xbar.author>
# <xbar.author.github>wansiedler</xbar.author.github>
# <xbar.desc>Your open ClickUp tasks grouped as overdue, due today, upcoming and undated, plus the running time tracker with a stop button. Click a task to open it.</xbar.desc>
# <xbar.dependencies>python3</xbar.dependencies>
# <xbar.abouturl>https://github.com/wansiedler/clickup-mac-widget</xbar.abouturl>
#
# <xbar.var>string(CLICKUP_TOKEN=""): Personal API token (pk_...). Leave empty to read it from the keychain instead</xbar.var>
# <xbar.var>string(CLICKUP_TEAM_ID=""): Workspace id; empty = first workspace</xbar.var>
#
# <swiftbar.hideAbout>true</swiftbar.hideAbout>
# <swiftbar.hideRunInTerminal>true</swiftbar.hideRunInTerminal>
"""SwiftBar / xbar plugin for ClickUp.

Token lookup order: $CLICKUP_TOKEN, then the login keychain item with service
"clickup-widget":

    security add-generic-password -s clickup-widget -a "$USER" -w 'pk_...'

Standard library only, so it runs on the python3 that ships with the Xcode CLT.
"""

from __future__ import annotations

import datetime as dt
import json
import os
import subprocess
import sys
import urllib.error
import urllib.parse
import urllib.request

API = "https://api.clickup.com/api/v2/"
APP_URL = "https://app.clickup.com/"
KEYCHAIN_SERVICE = "clickup-widget"
MAX_PAGES = 5
PER_SECTION = 15
PRIORITY_RANK = {"urgent": 1, "high": 2, "normal": 3, "low": 4}
RED = "#e5484d"
ORANGE = "#f76b15"


class ClickUpError(Exception):
    pass


def load_token() -> str | None:
    token = os.environ.get("CLICKUP_TOKEN", "").strip()
    if token:
        return token
    try:
        out = subprocess.run(
            ["/usr/bin/security", "find-generic-password", "-s", KEYCHAIN_SERVICE, "-w"],
            capture_output=True,
            text=True,
            timeout=10,
            check=False,
        )
    except (OSError, subprocess.TimeoutExpired):
        return None
    if out.returncode != 0:
        return None
    return out.stdout.strip() or None


def request(token: str, path: str, query: list[tuple[str, str]] | None = None, method: str = "GET") -> dict:
    url = API + path
    if query:
        url += "?" + urllib.parse.urlencode(query)
    req = urllib.request.Request(
        url,
        method=method,
        data=b"" if method != "GET" else None,
        headers={"Authorization": token, "Accept": "application/json"},
    )
    try:
        with urllib.request.urlopen(req, timeout=20) as resp:
            body = resp.read()
    except urllib.error.HTTPError as e:
        raise ClickUpError(f"HTTP {e.code} on {path}") from e
    except urllib.error.URLError as e:
        raise ClickUpError(f"network: {e.reason}") from e
    return json.loads(body) if body else {}


def resolve_team(token: str, override: str | None) -> str:
    if override:
        return override
    teams = request(token, "team").get("teams") or []
    if not teams:
        raise ClickUpError("no workspace visible to this token")
    return str(teams[0]["id"])


def fetch_tasks(token: str, team_id: str, user_id: int) -> list[dict]:
    tasks: list[dict] = []
    for page in range(MAX_PAGES):
        resp = request(
            token,
            f"team/{team_id}/task",
            [
                ("assignees[]", str(user_id)),
                ("subtasks", "true"),
                ("include_closed", "false"),
                ("order_by", "due_date"),
                ("page", str(page)),
            ],
        )
        batch = resp.get("tasks") or []
        tasks.extend(batch)
        if resp.get("last_page", len(batch) < 100):
            break
    return tasks


def fetch_timer(token: str, team_id: str) -> dict | None:
    try:
        data = request(token, f"team/{team_id}/time_entries/current").get("data")
    except ClickUpError:
        return None
    return data if data and data.get("id") else None


def due_of(task: dict) -> dt.datetime | None:
    raw = task.get("due_date")
    if not raw:
        return None
    try:
        return dt.datetime.fromtimestamp(int(raw) / 1000)
    except (TypeError, ValueError):
        return None


def urgency_key(task: dict) -> tuple:
    due = due_of(task)
    prio = (task.get("priority") or {}).get("priority")
    return (due is None, due or dt.datetime.max, PRIORITY_RANK.get(prio, 5))


def bucket(tasks: list[dict], now: dt.datetime) -> dict[str, list[dict]]:
    today = now.date()
    out: dict[str, list[dict]] = {"overdue": [], "today": [], "upcoming": [], "nodate": []}
    for task in sorted(tasks, key=urgency_key):
        due = due_of(task)
        if due is None:
            out["nodate"].append(task)
        elif due.date() < today:
            out["overdue"].append(task)
        elif due.date() == today:
            out["today"].append(task)
        else:
            out["upcoming"].append(task)
    return out


def clean(text: str) -> str:
    # "|" starts SwiftBar params, a leading "-" reads as a submenu marker.
    return (text or "").replace("|", "¦").replace("\n", " ").lstrip("-").strip() or "(untitled)"


def elapsed(start_ms: str | int | None, now: dt.datetime) -> str:
    try:
        start = dt.datetime.fromtimestamp(int(start_ms) / 1000)
    except (TypeError, ValueError):
        return "?"
    minutes = max(0, int((now - start).total_seconds() // 60))
    return f"{minutes // 60}:{minutes % 60:02d}"


def task_lines(task: dict, prefix: str, color: str | None) -> list[str]:
    params = f"href={task.get('url') or APP_URL}"
    if color:
        params += f" color={color}"
    lines = [f"{prefix}{clean(task.get('name', ''))} | {params}"]
    details = [clean((task.get("list") or {}).get("name", "")), clean((task.get("status") or {}).get("status", ""))]
    due = due_of(task)
    if due:
        details.append(due.strftime("due %Y-%m-%d"))
    lines.append(f"{prefix}--{' · '.join(d for d in details if d != '(untitled)')} | size=11")
    return lines


def render(buckets: dict[str, list[dict]], timer: dict | None, now: dt.datetime, script: str) -> list[str]:
    due_now = len(buckets["overdue"]) + len(buckets["today"])
    title = str(due_now)
    if timer:
        title = f"{elapsed(timer.get('start'), now)} · {title}"
    params = f"sfimage={'timer' if timer else 'checklist'}"
    if buckets["overdue"]:
        params += f" color={RED}"
    lines = [f"{title} | {params}", "---"]

    if timer:
        task = timer.get("task") or {}
        name = clean(task.get("name") or timer.get("description") or "No task")
        href = f"{APP_URL}t/{task['id']}" if task.get("id") else APP_URL
        lines.append(f"Timer {elapsed(timer.get('start'), now)} · {name} | href={href} sfimage=timer")
        lines.append(f'Stop timer | shell="{script}" param1=stop terminal=false refresh=true sfimage=stop.fill')
        lines.append("---")

    for key, label, color in (("overdue", "Overdue", RED), ("today", "Today", ORANGE)):
        if buckets[key]:
            lines.append(f"{label} ({len(buckets[key])}) | color={color}")
            for task in buckets[key][:PER_SECTION]:
                lines += task_lines(task, "", None)
            lines.append("---")
    if due_now == 0:
        lines += ["Nothing due today", "---"]

    for key, label in (("upcoming", "Upcoming"), ("nodate", "No due date")):
        if buckets[key]:
            lines.append(f"{label} ({len(buckets[key])})")
            for task in buckets[key][:PER_SECTION]:
                lines += task_lines(task, "--", None)

    lines += [
        "---",
        f"Open ClickUp | href={APP_URL}",
        "Refresh | refresh=true",
        f"Updated {now:%H:%M} | size=11",
    ]
    return lines


def main(argv: list[str]) -> int:
    script = os.path.abspath(argv[0])
    token = load_token()
    if not token:
        print("ClickUp ? | sfimage=key")
        print("---")
        print("No API token found")
        print("ClickUp → Settings → Apps → API Token, then:")
        print(f"security add-generic-password -s {KEYCHAIN_SERVICE} -a $USER -w 'pk_…' | font=Menlo size=11")
        return 0

    team_override = os.environ.get("CLICKUP_TEAM_ID", "").strip() or None
    try:
        team_id = resolve_team(token, team_override)
        if len(argv) > 1 and argv[1] == "stop":
            request(token, f"team/{team_id}/time_entries/stop", method="POST")
            return 0
        user_id = request(token, "user")["user"]["id"]
        tasks = fetch_tasks(token, team_id, user_id)
        timer = fetch_timer(token, team_id)
    except (ClickUpError, KeyError) as e:
        print("ClickUp ! | sfimage=exclamationmark.triangle")
        print("---")
        print(clean(str(e)))
        print("Refresh | refresh=true")
        return 0

    now = dt.datetime.now()
    print("\n".join(render(bucket(tasks, now), timer, now, script)))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
