#!/usr/bin/env python3

# <xbar.title>Live Tennis Scores</xbar.title>
# <xbar.version>v3.0</xbar.version>
# <xbar.author>Ben, Anup Sam Abraham</xbar.author>
# <xbar.author.github>bensynapse,anupsabraham</xbar.author.github>
# <xbar.desc>Live tennis scores (ATP / WTA / Challenger / ITF) in your menu bar, powered by the official Live Tennis API. Shows sets, games, current-game points and who is serving. Free API key at livetennisapi.com (1,000 requests/day — the 2m refresh uses ~720).</xbar.desc>
# <xbar.image>https://i.postimg.cc/BQq9CSgv/SCR-20231231-mlck.png</xbar.image>
# <xbar.dependencies>python3</xbar.dependencies>
# <xbar.abouturl>https://livetennisapi.com</xbar.abouturl>

# <xbar.var>string(VAR_LIVETENNIS_API_KEY=""): Live Tennis API key. Get a free one at https://livetennisapi.com/subscribe/free</xbar.var>
# <xbar.var>select(VAR_TOUR_FILTER="all"): Which tour to show. [all, atp, wta, challenger, itf]</xbar.var>

# v3.0 (2026): rewritten to use the official Live Tennis API
# (https://livetennisapi.com). The previous version scraped an internal
# atptour.com endpoint which is now behind a Cloudflare challenge and
# returns 403, so the plugin had stopped working. Refresh cadence moved
# from 1m to 2m to stay inside the free tier's 1,000 requests/day.

import json
import os
import urllib.error
import urllib.request

API_URL = "https://api.livetennisapi.com/api/public/v1/matches?status=live&limit=50"
SIGNUP_URL = "https://livetennisapi.com/subscribe/free"
SITE_URL = "https://livetennisapi.com"

BALL = "\U0001F3BE"  # tennis ball emoji


def menu_bar(text):
    print(text)
    print("---")


def fail(bar_suffix, *dropdown_lines):
    menu_bar(BALL + " " + bar_suffix)
    for line in dropdown_lines:
        print(line)
    print("Refresh | refresh=true")


def fetch_matches(api_key):
    # The API's edge blocks the default Python-urllib User-Agent, so
    # identify ourselves as the plugin instead.
    headers = {"x-api-key": api_key, "User-Agent": "xbar-live-tennis/3.0"}
    req = urllib.request.Request(API_URL, headers=headers)
    with urllib.request.urlopen(req, timeout=15) as resp:
        return json.loads(resp.read().decode("utf-8"))


def tour_of(match):
    """Best-effort tour label for a match ('atp', 'wta', 'challenger', 'itf', or '')."""
    # ITF events are named by prize level ("W15 …", "M25 …"), and their
    # players still carry ATP/WTA rankings — classify those by name first.
    name = match.get("tournament") or ""
    if name[:1] in ("W", "M") and name[1:3].isdigit():
        return "itf"
    players = match.get("players") or {}
    for side in ("p1", "p2"):
        tour = ((players.get(side) or {}).get("tour") or "").strip().lower()
        if tour:
            return tour
    return ""


def player_line(match, side_idx):
    players = match.get("players") or {}
    player = players.get("p1" if side_idx == 0 else "p2") or {}
    score = match.get("score") or {}

    name = player.get("name") or "Unknown"
    ranking = player.get("ranking")
    if ranking and not player.get("is_doubles_team"):
        name += " (#{})".format(ranking)

    games = score.get("games") or [[], []]
    own_games = games[side_idx] if len(games) > side_idx else []
    sets_str = " ".join(str(g) for g in own_games) or "0"

    parts = [sets_str]
    points = score.get("points") or []
    if len(points) > side_idx and points[side_idx] not in (None, ""):
        pts = str(points[side_idx])
        if score.get("is_tiebreak"):
            pts += " TB"
        parts.append(pts)

    serve = BALL + " " if score.get("server") == side_idx + 1 else "  "
    return "{}{} — {}".format(serve, name, " · ".join(parts))


def main():
    api_key = (os.environ.get("VAR_LIVETENNIS_API_KEY") or "").strip()
    tour_filter = (os.environ.get("VAR_TOUR_FILTER") or "all").strip().lower()

    if not api_key:
        fail(
            "⚙︎",
            "No API key set — open the plugin settings in xbar | color=red",
            "Get a free API key (1,000 requests/day) | href=" + SIGNUP_URL,
        )
        return

    try:
        payload = fetch_matches(api_key)
    except urllib.error.HTTPError as e:
        if e.code == 401:
            fail(
                "⚠️",
                "Invalid API key (HTTP 401) | color=red",
                "Check VAR_LIVETENNIS_API_KEY in the plugin settings",
                "Get a free API key | href=" + SIGNUP_URL,
            )
        elif e.code == 429:
            fail(
                "⏳",
                "API quota exceeded (HTTP 429) | color=red",
                "Free tier is 1,000 requests/day — scores resume when the quota resets",
                "Manage your plan | href=" + SITE_URL,
            )
        else:
            fail("⚠️", "Live Tennis API error: HTTP {} | color=red".format(e.code))
        return
    except (urllib.error.URLError, TimeoutError, json.JSONDecodeError, OSError):
        fail("—", "Could not reach the Live Tennis API (offline?) | color=gray")
        return

    matches = payload.get("data") or []
    if tour_filter != "all":
        matches = [m for m in matches if tour_of(m) == tour_filter]

    if not matches:
        label = "" if tour_filter == "all" else " {}".format(tour_filter.upper())
        menu_bar(BALL)
        print("No live{} matches right now".format(label))
        print("Live Tennis API | href=" + SITE_URL)
        print("Refresh | refresh=true")
        return

    menu_bar("{} {}".format(BALL, len(matches)))

    # Group matches by tournament, preserving API order.
    by_tournament = {}
    for match in matches:
        by_tournament.setdefault(match.get("tournament") or "Unknown tournament", []).append(match)

    for tournament, t_matches in by_tournament.items():
        print("{} | size=12 color=gray href={}".format(tournament, SITE_URL))
        for match in t_matches:
            round_name = match.get("round") or ""
            # Round strings arrive as "W15 Nogent-sur-Marne - Quarter-finals";
            # drop the leading tournament repeat when present. The tournament
            # field sometimes adds a country suffix ("W100 Amstetten (Austria)")
            # that the round string omits, so try that form too.
            bare = tournament.split(" (")[0]
            for prefix in (tournament + " - ", bare + " - "):
                if round_name.startswith(prefix):
                    round_name = round_name[len(prefix):]
                    break
            fmt = match.get("format") or ""
            detail = " · ".join(x for x in (round_name, fmt) if x)
            if detail:
                print("{} | size=11 color=#888888 length=60".format(detail))
            print(player_line(match, 0) + " | font=Menlo size=12 length=60")
            print(player_line(match, 1) + " | font=Menlo size=12 length=60")
        print("---")

    print("Powered by Live Tennis API | href=" + SITE_URL)
    print("Refresh | refresh=true")


if __name__ == "__main__":
    main()
