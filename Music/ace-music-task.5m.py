#!/usr/bin/env python3
# <xbar.title>ACE Music Task Monitor</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Ace Data Cloud</xbar.author>
# <xbar.author.github>acedatacloud-dev</xbar.author.github>
# <xbar.desc>Follow an existing music task from the menu bar and open its available tracks, including partial results.</xbar.desc>
# <xbar.image>https://cdn.acedata.cloud/acemusic-icon-52deddfc87b9.png</xbar.image>
# <xbar.dependencies>python3</xbar.dependencies>
# <xbar.abouturl>https://github.com/AceDataCloud/AceMusicMCP</xbar.abouturl>
# <xbar.var>string(VAR_TASK_ID=""): ID of the ACE Music task to watch. Clear it to stop polling.</xbar.var>

import getpass
import json
import os
import subprocess
from urllib.error import HTTPError, URLError
from urllib.parse import urlsplit
from urllib.request import HTTPRedirectHandler, Request, build_opener


class NoRedirect(HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


def keychain_token():
    result = subprocess.run(
        ["/usr/bin/security", "find-generic-password", "-s", "ACE Music xbar",
         "-a", getpass.getuser(), "-w"],
        capture_output=True, text=True, timeout=10,
    )
    return result.stdout.strip() if result.returncode == 0 else ""


def retrieve(task_id, token):
    request = Request(
        "https://api.acedata.cloud/acemusic/tasks",
        data=json.dumps({"action": "retrieve", "id": task_id}).encode(),
        headers={"Authorization": "Bearer " + token, "Content-Type": "application/json"},
    )
    with build_opener(NoRedirect()).open(request, timeout=15) as response:
        return json.load(response)


def display_text(text):
    return " ".join("".join(c if c.isprintable() else " " for c in text)
                    .replace("|", "·").split())[:110]


def track_link(item):
    if not isinstance(item, dict):
        return None
    url = item.get("audio_url")
    if not isinstance(url, str) or any(
        c.isspace() or not c.isprintable() or c in "\"'|\\" for c in url
    ):
        return None
    try:
        parts = urlsplit(url)
        if parts.scheme == "https" and parts.hostname and parts.username is None and parts.password is None:
            return url
    except ValueError:
        pass
    return None


def render(result, task_id):
    if not result:
        return "Not found", ["No task with this ID was found for this account."]
    group = result.get("response") or {}
    if result.get("id") != task_id or group.get("task_id") not in (None, task_id):
        return "Lookup failed", ["The response did not match the requested task."]
    state = group.get("status") or result.get("state") or result.get("status") or "pending"
    labels = {"partial_failed": "Partial result", "submission_unknown": "Submission unconfirmed"}
    state = labels.get(state, display_text(str(state)).replace("_", " ").capitalize())
    tracks = []
    for track in group.get("data") or []:
        url = track_link(track)
        if url:
            label = display_text(str(track.get("title") or "Untitled track"))
            tracks.append(f"Play {label} | href={url} emojize=false ansi=false")
    return state, tracks or ["No playable audio available."]


def menu(task_id):
    if not task_id:
        return "Set up", ["Set the task ID in the plugin preferences to begin."]
    token = keychain_token()
    if not token:
        return "Set up", [
            "In Keychain Access, add a new password item:",
            "Name: ACE Music xbar",
            "Account: your macOS short username",
            "Password: your ACE Music task-lookup API key",
            "Get an application key | href=https://platform.acedata.cloud/console/applications",
        ]
    try:
        return render(retrieve(task_id, token), task_id)
    except HTTPError as error:
        return "Lookup failed", [f"Task lookup returned HTTP {error.code}."]
    except (URLError, TimeoutError, ValueError, TypeError, AttributeError):
        return "Lookup failed", ["Could not read the music task. Refresh to retry."]


if __name__ == "__main__":
    try:
        status, items = menu(os.environ.get("VAR_TASK_ID", "").strip())
    except (OSError, subprocess.SubprocessError):
        status, items = "Keychain unavailable", ["Check Keychain Access and refresh."]
    print("ACE Music · " + status)
    print("---")
    for item in items:
        print(item)
    print("---")
    print("Refresh now | refresh=true")
