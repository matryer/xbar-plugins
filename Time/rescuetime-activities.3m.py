#!/usr/bin/env -S PATH="/opt/homebrew/Caskroom/mambaforge/base/bin:/opt/conda/bin:/opt/miniconda3/bin:/opt/homebrew/bin:/usr/local/bin:${PATH}" PYTHONIOENCODING=UTF-8 python3
# <xbar.title>RescueTime Activities</xbar.title>
# <xbar.version>v1.6</xbar.version>
# <xbar.author>Piotr Migdał</xbar.author>
# <xbar.author.github>stared</xbar.author.github>
# <xbar.desc>Displays a summary of your RescueTime activities directly in the status bar, allowing you to monitor productivity levels at a glance.</xbar.desc>
# <xbar.dependencies>python, Pillow (optional)</xbar.dependencies>
# <xbar.image>https://raw.githubusercontent.com/stared/xbar-rescuetime-activities/refs/heads/main/xbar-rescuetime-activities-screenshot.png</xbar.image>
# <xbar.abouturl>https://github.com/stared/xbar-rescuetime-activities</xbar.abouturl>
# <xbar.var>string(VAR_RESCUETIME_API_KEY=""): RescueTime API key - create at https://www.rescuetime.com/anapi/manage</xbar.var>
# <xbar.var>number(VAR_ACTIVITIES_LIMIT="15"): Limit the number of activities to show.</xbar.var>

import sys
import os
import json
import datetime
import urllib.parse
import urllib.request

MAPPING_COLOR = {
    2: "#27ae60",  # Dark green - very productive
    1: "#7ecc71",  # Light green - productive
    0: "#3498db",  # Blue - neutral
    -1: "#e67e22",  # Orange - distracting
    -2: "#e74c3c",  # Red - very distracting
}


def load_api_key() -> str:
    """Load the API key from environment variable VAR_RESCUETIME_API_KEY."""
    api_key = os.environ.get("VAR_RESCUETIME_API_KEY")
    if not api_key:
        print("X")
        print("---")
        print("Missing API Key")
        print(
            "Generate an API key in RescueTime | href=https://www.rescuetime.com/anapi/manage"
        )
        print("And set it in the plugin settings:")
        print("xbar > Open Plugin > RescueTime API Key (or ⌘+E)")
        exit()
    return api_key


def fetch_data(url: str, params: dict) -> dict:
    """Fetch data from the given URL with parameters."""
    query_params = urllib.parse.urlencode(params)
    full_url = f"{url}?{query_params}"
    try:
        with urllib.request.urlopen(full_url) as response:
            return json.loads(response.read())
    except Exception as e:
        print(f"Error fetching data: {e}")
        return {}


def rescuetime_activity_data(params: str) -> list[dict]:
    """See RescueTime API docs for details, https://www.rescuetime.com/rtx/developers."""
    result = fetch_data("https://www.rescuetime.com/anapi/data", params)

    if not result or "rows" not in result:
        print("No data available.")
        return []

    row_headers = result.get("row_headers", [])
    rows = result.get("rows", [])
    return [dict(zip(row_headers, row)) for row in rows]


def format_time(seconds: int) -> str:
    """Format seconds into hours and minutes, e.g., '2h 13m'."""
    hours = seconds // 3600
    minutes = (seconds % 3600) // 60
    if hours > 0:
        return f"{hours}h {minutes}m"
    else:
        return f"{minutes}m"


def create_chart_image(
    hours_data: dict, width: int = 200, height: int = 40, opacity: int = 127
) -> str:
    """Create a minimalistic stacked bar chart image."""
    from PIL import Image, ImageDraw
    import base64
    from io import BytesIO

    img = Image.new("RGBA", (width, height), (0, 0, 0, 0))
    draw = ImageDraw.Draw(img)

    max_total = max(sum(prod_times.values()) for prod_times in hours_data.values())

    def hex_to_rgba(hex_color: str, opacity: int) -> tuple:
        """Convert hex color to RGBA tuple."""
        rgb = tuple(int(hex_color[i : i + 2], 16) for i in (1, 3, 5))
        return rgb + (opacity,)

    colors = {
        level: hex_to_rgba(MAPPING_COLOR[level], opacity) for level in range(-2, 3)
    }

    bar_width = width // 24
    for hour in range(24):
        if hour not in hours_data or not any(hours_data[hour].values()):
            continue

        x = hour * bar_width
        y_bottom = height

        # Stack the bars for each productivity level
        for prod in [2, 1, 0, -1, -2]:
            if hours_data[hour][prod] > 0:
                bar_height = int((hours_data[hour][prod] / max_total) * height)
                y_top = y_bottom - bar_height

                draw.rectangle(
                    [x, y_top, x + bar_width, y_bottom],
                    fill=colors[prod],
                )
                y_bottom = y_top

    buffer = BytesIO()
    img.save(buffer, format="PNG")
    return base64.b64encode(buffer.getvalue()).decode()


def get_hourly_productivity_data(key: str, date_str: str) -> dict:
    """Fetch and process hourly productivity data."""
    params = {
        "key": key,
        "restrict_kind": "productivity",
        "interval": "hour",
        "perspective": "interval",
        "restrict_begin": date_str,
        "restrict_end": date_str,
        "format": "json",
    }

    result = fetch_data("https://www.rescuetime.com/anapi/data", params)

    if not result or "rows" not in result:
        return {}

    hours_data = {i: {p: 0 for p in range(-2, 3)} for i in range(24)}

    for row in result["rows"]:
        hour = int(row[0].split("T")[1][:2])
        seconds = row[1]
        productivity = row[3]
        hours_data[hour][productivity] += seconds

    return hours_data


def main() -> None:
    key = load_api_key()
    date_str = datetime.date.today().strftime("%Y-%m-%d")

    activities = rescuetime_activity_data(
        {
            "format": "json",
            "key": key,
            "resolution_time": "day",
            "restrict_begin": date_str,
            "restrict_end": date_str,
            "restrict_kind": "activity",
        }
    )
    pulse = fetch_data(
        "https://www.rescuetime.com/anapi/current_productivity_pulse.json",
        params={"key": key},
    )
    productive_seconds = sum(
        activity["Time Spent (seconds)"]
        for activity in activities
        if activity["Productivity"] > 0
    )

    print_header(productive_seconds, pulse)
    print_productivity_totals(activities)
    print_top_activities(activities)

    hours_data = get_hourly_productivity_data(key, date_str)
    print_hourly_chart(hours_data)

    print_notes()


def print_header(productive_seconds: int, pulse: dict) -> None:
    print(f"{format_time(productive_seconds)} | color={pulse.get('color', '#000000')}")
    print("---")
    print("RescueTime Activites | href=https://www.rescuetime.com/dashboard")


def print_productivity_totals(activities: list[dict]) -> None:
    categories = [
        ("Productive", lambda p: p > 0, 2),
        ("Neutral", lambda p: p == 0, 0),
        ("Distracting", lambda p: p < 0, -2),
    ]

    for name, condition, p_value in categories:
        total_seconds = sum(
            activity["Time Spent (seconds)"]
            for activity in activities
            if condition(activity["Productivity"])
        )
        formatted_name = f"{name}:".ljust(12)
        print(
            f"{formatted_name} {format_time(total_seconds):>6} | "
            f"font='Menlo' size=12 color={MAPPING_COLOR[p_value]}"
        )
    print("---")


def print_top_activities(activities: list[dict]) -> None:
    print("Top activities")
    activities_limit = int(os.environ.get("VAR_ACTIVITIES_LIMIT", 15))
    for activity in activities[:activities_limit]:
        seconds = activity["Time Spent (seconds)"]
        name = activity["Activity"]
        productivity = activity["Productivity"]
        print(
            f"{format_time(seconds):>6} {name} | "
            f"font='Menlo' size=12 trim=false color={MAPPING_COLOR[productivity]}"
        )


def print_hourly_chart(hours_data: dict) -> None:
    print("---")
    print("Productivity by hour")
    try:
        base64_img = create_chart_image(hours_data)
        print(f"| image={base64_img}")
    except ImportError:
        print("---")
        print("To see daily chart install Pillow")
        print("-- most likely you need to run:")
        print(f"-- {sys.executable} -m pip install Pillow")
        print("Then xbar > Refresh (or ⌘+R)")


def print_notes() -> None:
    print("---")
    print("Notes")
    print("-- RescueTime data syncs every 3 min (Premium) or 30 min (Free)")
    print("-- Activities shown are from the last 24 hours")
    print("-- Colors indicate productivity level")
    print("-- By Piotr Migdał (2024) | href=https://p.migdal.pl")
    print(
        "-- Source code available | "
        "href=https://github.com/stared/xbar-rescuetime-activities"
    )
    print("-- System info")
    print(f"-- Python path: {sys.executable}")
    print(f"-- Python version: {sys.version.split()[0]}")


if __name__ == "__main__":
    main()
