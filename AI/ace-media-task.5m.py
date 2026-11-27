#!/usr/bin/env python3
# <xbar.title>Ace Media Task Monitor</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.author>Ace Data Cloud</xbar.author>
# <xbar.author.github>acedatacloud-dev</xbar.author.github>
# <xbar.desc>Follow an existing image, music or video generation task and open completed output from the menu bar.</xbar.desc>
# <xbar.image>https://cdn.acedata.cloud/uploads/ecosystem-xbar-media-task-menu-dcb10e7f7e08d0f7.png</xbar.image>
# <xbar.dependencies>python3</xbar.dependencies>
# <xbar.abouturl>https://docs.acedata.cloud/</xbar.abouturl>
# <xbar.var>select(VAR_SERVICE="ACE Music"): Service that created the task. [ACE Music, Suno, Nano Banana, GPT Image, Seedream, FLUX, Seedance, Kling, Veo, Hailuo]</xbar.var>
# <xbar.var>string(VAR_TASK_ID=""): ID of a task created by the selected service. Clear it to stop polling.</xbar.var>

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


# Service names are taken from the public API catalog. Restrict lookups to
# these task endpoints even if a preference is edited outside xbar.
SERVICES = {
    "ACE Music": ("acemusic", "audio_url", "Play"),
    "Suno": ("suno", "audio_url", "Play"),
    "Nano Banana": ("nano-banana", "image_url", "View"),
    "GPT Image": ("openai", "url", "View"),
    "Seedream": ("seedream", "image_url", "View"),
    "FLUX": ("flux", "image_url", "View"),
    "Seedance": ("seedance", "video_url", "Watch"),
    "Kling": ("kling", "video_url", "Watch"),
    "Veo": ("veo", "video_url", "Watch"),
    "Hailuo": ("hailuo", "video_url", "Watch"),
}


def keychain_token(service):
    result = subprocess.run(
        ["/usr/bin/security", "find-generic-password", "-s", service + " xbar",
         "-a", getpass.getuser(), "-w"],
        capture_output=True, text=True, timeout=10,
    )
    return result.stdout.strip() if result.returncode == 0 else ""


def retrieve(task_id, token, service):
    request = Request(
        f"https://api.acedata.cloud/{SERVICES[service][0]}/tasks",
        data=json.dumps({"action": "retrieve", "id": task_id}).encode(),
        headers={"Authorization": "Bearer " + token, "Content-Type": "application/json"},
    )
    with build_opener(NoRedirect()).open(request, timeout=15) as response:
        return json.load(response)


def display_text(text):
    return " ".join("".join(c if c.isprintable() else " " for c in text)
                    .replace("|", "·").split())[:110]


def asset_link(item, field):
    if not isinstance(item, dict):
        return None
    url = item.get(field)
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


def render(result, task_id, service):
    if not result:
        return "Not found", ["No task with this ID was found for this account."]
    group = result.get("response") or {}
    if result.get("id") != task_id or group.get("task_id") not in (None, task_id):
        return "Lookup failed", ["The response did not match the requested task."]
    slug, field, action = SERVICES[service]
    data = group.get("data") or []
    if slug == "seedance":
        rows = [data.get("content")] if isinstance(data, dict) else []
    elif slug == "kling":
        rows = [group]
    else:
        rows = data if isinstance(data, list) else []
    rows = [row for row in rows if isinstance(row, dict)]
    state = group.get("status") or group.get("state")
    if not state and isinstance(data, dict):
        state = data.get("status")
    state = state or result.get("state") or result.get("status")
    if not state:
        states = {row.get("state") or row.get("status") for row in rows}
        states.discard(None)
        if group.get("success") is False or group.get("error"):
            state = "failed"
        elif len(states) == 1:
            state = states.pop()
        elif states:
            state = "mixed"
        elif result.get("finished_at") or group.get("finished_at") or group.get("success") is True:
            state = "completed"
        else:
            state = "pending"
    labels = {"partial_failed": "Partial result", "submission_unknown": "Submission unconfirmed"}
    state = labels.get(state, display_text(str(state)).replace("_", " ").capitalize())
    items = []
    for index, row in enumerate(rows, start=1):
        url = asset_link(row, field)
        if url:
            label = display_text(str(row.get("title") or f"result {index}"))
            items.append(f"{action} {label} | href={url} emojize=false ansi=false")
        elif slug in ("openai", "seedream") and row.get("b64_json"):
            items.append(f"Image {index} is embedded. Retrieve the original task to save it.")
    return state, items or ["No output link is available yet."]


def menu(task_id, service):
    if service not in SERVICES:
        return "Set up", ["Select a supported service in the plugin preferences."]
    if not task_id:
        return "Set up", ["Choose a service and set the existing task ID in the plugin preferences."]
    token = keychain_token(service)
    if not token:
        return "Set up", [
            "In Keychain Access, add a new password item:",
            "Name: " + service + " xbar",
            "Account: your macOS short username",
            "Password: a key authorized for task lookup for this service",
            "Get an application key | href=https://platform.acedata.cloud/console/applications?utm_source=xbar&utm_medium=referral&utm_campaign=evergreen",
        ]
    try:
        return render(retrieve(task_id, token, service), task_id, service)
    except HTTPError as error:
        return "Lookup failed", [f"Task lookup returned HTTP {error.code}."]
    except (URLError, TimeoutError, ValueError, TypeError, AttributeError):
        return "Lookup failed", ["Could not read the task. Refresh to retry."]


if __name__ == "__main__":
    service = os.environ.get("VAR_SERVICE", "ACE Music")
    try:
        status, items = menu(os.environ.get("VAR_TASK_ID", "").strip(), service)
    except (OSError, subprocess.SubprocessError):
        status, items = "Keychain unavailable", ["Check Keychain Access and refresh."]
    print((service if service in SERVICES else "Ace Media") + " · " + status)
    print("---")
    for item in items:
        print(item)
    print("---")
    print("Refresh now | refresh=true")
