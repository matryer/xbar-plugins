#!/usr/bin/env python3
# <xbar.title>Codex limits</xbar.title>
# <xbar.version>v1.2.0</xbar.version>
# <xbar.author>Pablo Guil</xbar.author>
# <xbar.author.github>pabloguil</xbar.author.github>
# <xbar.desc>Shows Codex usage limits from local Codex session data.</xbar.desc>
# <xbar.dependencies>python3</xbar.dependencies>
# <xbar.abouturl>https://github.com/pabloguil/codex-usage-swiftbar</xbar.abouturl>

import datetime as dt
import base64
import json
import locale
import os
import subprocess
import sys
import time
import math
import struct
import zlib
from pathlib import Path


CODEX_HOME = Path(os.environ.get("CODEX_HOME", Path.home() / ".codex"))
SESSIONS_DIR = CODEX_HOME / "sessions"
ARCHIVED_DIR = CODEX_HOME / "archived_sessions"
STATE_PATH = CODEX_HOME / "swiftbar-codex-limits-state.json"
USAGE_URL = "https://chatgpt.com/codex/settings/usage"
PRICING_URL = "https://developers.openai.com/codex/pricing"

COLORS = {
    "green": (48, 209, 88, 255),
    "orange": (255, 159, 10, 255),
    "red": (255, 69, 58, 255),
    "gray": (142, 142, 147, 255),
}

SYSTEM_LANGUAGE = "en"

TEXT = {
    "en": {
        "attention": "Codex: attention",
        "critical_limit": "Codex: critical limit",
        "codex_no_data": "Codex no data",
        "codex_error": "Codex error",
        "credits": "Credits",
        "credits_no_extra": "no extra credits",
        "credits_unlimited": "unlimited",
        "data_updated": "Data updated {minutes} min ago",
        "display_mode": "Menu bar display",
        "estimated_start": "Estimated start",
        "five_hour_limit": "5-hour limit",
        "folder_checked": "Folder checked",
        "high": "high",
        "no_data": "no data",
        "normal": "normal",
        "official_usage": "Open official usage",
        "open_codex_usage": "Open Codex usage",
        "only_five": "Only 5-hour limit",
        "only_weekly": "Only weekly limit",
        "pace": "Pace",
        "pace_extra": ", pace {pace}",
        "plan": "Plan",
        "pricing": "View Codex pricing",
        "proportional": "proportional",
        "refresh_swiftbar": "Refresh SwiftBar",
        "remaining": "remaining",
        "reset": "Reset",
        "reviewed_folder": "I cannot find Codex limit data.",
        "source": "Source",
        "settings": "Settings",
        "show_combined": "Combined",
        "show_separate": "Two indicators",
        "time_left": "Time left",
        "used": "Used",
        "used_vs": "{used:.0f}% used vs {expected:.0f}% proportional, {sign}{diff:.0f} pts",
        "unknown": "unknown",
        "very_high": "very high",
        "weekly_limit": "Weekly limit",
        "weekly_short": "Week",
        "quiet": "quiet",
    },
    "es": {
        "attention": "Codex: atención",
        "critical_limit": "Codex: límite crítico",
        "codex_no_data": "Codex sin datos",
        "codex_error": "Codex error",
        "credits": "Créditos",
        "credits_no_extra": "sin créditos extra",
        "credits_unlimited": "ilimitados",
        "data_updated": "Dato actualizado hace {minutes} min",
        "display_mode": "Vista en la barra",
        "estimated_start": "Inicio estimado",
        "five_hour_limit": "Límite de 5 horas",
        "folder_checked": "Carpeta revisada",
        "high": "alto",
        "no_data": "sin dato",
        "normal": "normal",
        "official_usage": "Abrir uso oficial",
        "open_codex_usage": "Abrir uso de Codex",
        "only_five": "Solo límite de 5 horas",
        "only_weekly": "Solo límite semanal",
        "pace": "Ritmo",
        "pace_extra": ", ritmo {pace}",
        "plan": "Plan",
        "pricing": "Ver precios Codex",
        "proportional": "proporcional",
        "refresh_swiftbar": "Refrescar SwiftBar",
        "remaining": "restante",
        "reset": "Reinicio",
        "reviewed_folder": "No encuentro datos de límites en Codex.",
        "source": "Fuente",
        "settings": "Ajustes",
        "show_combined": "Combinado",
        "show_separate": "Dos indicadores",
        "time_left": "Tiempo restante",
        "used": "Usado",
        "used_vs": "{used:.0f}% usado vs {expected:.0f}% proporcional, {sign}{diff:.0f} pts",
        "unknown": "desconocido",
        "very_high": "muy alto",
        "weekly_limit": "Límite semanal",
        "weekly_short": "Sem",
        "quiet": "tranquilo",
    },
}


def t(key, **values):
    text = TEXT.get(SYSTEM_LANGUAGE, TEXT["en"]).get(key, TEXT["en"].get(key, key))
    return text.format(**values) if values else text


def configure_locale():
    global SYSTEM_LANGUAGE
    candidates = []
    forced_language = os.environ.get("CODEX_LIMITS_LANG", "").strip().lower()
    if forced_language:
        SYSTEM_LANGUAGE = forced_language.split("_", 1)[0].split("-", 1)[0]
        if SYSTEM_LANGUAGE not in TEXT:
            SYSTEM_LANGUAGE = "en"
        if SYSTEM_LANGUAGE == "es":
            candidates.extend(["es_ES.UTF-8", "es_ES"])
        elif SYSTEM_LANGUAGE == "en":
            candidates.extend(["en_US.UTF-8", "en_US"])

    try:
        apple_locale = subprocess.run(
            ["defaults", "read", "-g", "AppleLocale"],
            capture_output=True,
            text=True,
            timeout=2,
            check=False,
        ).stdout.strip()
        if apple_locale:
            if not forced_language:
                SYSTEM_LANGUAGE = apple_locale.split("_", 1)[0].split("-", 1)[0].lower()
                if SYSTEM_LANGUAGE not in TEXT:
                    SYSTEM_LANGUAGE = "en"
            candidates.append(apple_locale)
            candidates.append(f"{apple_locale}.UTF-8")
    except Exception:
        pass

    candidates.extend(["", "es_ES.UTF-8", "es_ES"])
    for candidate in candidates:
        try:
            locale.setlocale(locale.LC_TIME, candidate)
            return
        except Exception:
            continue


configure_locale()


def local_now():
    return dt.datetime.now().astimezone()


def parse_time(value):
    if not value:
        return 0.0
    try:
        return dt.datetime.fromisoformat(value.replace("Z", "+00:00")).timestamp()
    except Exception:
        return 0.0


def iter_recent_files():
    files = []
    for base in (SESSIONS_DIR, ARCHIVED_DIR):
        if not base.exists():
            continue
        try:
            files.extend(p for p in base.rglob("*.jsonl") if p.is_file())
        except Exception:
            pass
    return sorted(files, key=lambda p: p.stat().st_mtime, reverse=True)[:80]


def find_latest_rate_limits():
    latest = None
    latest_ts = 0.0
    latest_file = None

    for path in iter_recent_files():
        try:
            with path.open("r", encoding="utf-8", errors="ignore") as f:
                for line in f:
                    if "rate_limits" not in line:
                        continue
                    try:
                        item = json.loads(line)
                    except Exception:
                        continue
                    payload = item.get("payload") or {}
                    rate_limits = payload.get("rate_limits")
                    if not isinstance(rate_limits, dict):
                        continue
                    ts = parse_time(item.get("timestamp")) or path.stat().st_mtime
                    if ts >= latest_ts:
                        latest = rate_limits
                        latest_ts = ts
                        latest_file = path
        except Exception:
            continue

    return latest, latest_ts, latest_file


def normalize_bucket(raw):
    if not isinstance(raw, dict):
        return None

    used = raw.get("used_percent")
    if used is None:
        used = raw.get("usedPercent")

    minutes = raw.get("window_minutes")
    if minutes is None:
        seconds = raw.get("limit_window_seconds")
        minutes = seconds / 60 if isinstance(seconds, (int, float)) else raw.get("windowDurationMins")

    reset = raw.get("resets_at")
    if reset is None:
        reset = raw.get("reset_at")
    if reset is None:
        reset = raw.get("resetsAt")

    try:
        used = float(used)
    except Exception:
        used = 0.0

    try:
        minutes = float(minutes)
    except Exception:
        minutes = 0.0

    try:
        reset = float(reset)
    except Exception:
        reset = None

    remaining = max(0.0, min(100.0, 100.0 - used))
    return {
        "used": max(0.0, min(100.0, used)),
        "remaining": remaining,
        "minutes": minutes,
        "reset": reset,
    }


def time_left_text(timestamp, now=None, short=False):
    if not timestamp:
        return t("no_data")
    now_ts = (now or time.time())
    seconds = max(0, int(timestamp - now_ts))
    minutes = max(1, round(seconds / 60))
    hours = round(seconds / 3600)
    days = round(seconds / 86400)

    if minutes < 60:
        if short:
            return f"{minutes}m"
        if SYSTEM_LANGUAGE == "es":
            return f"queda {minutes} min"
        return f"{minutes} min left"
    if hours < 48:
        if short:
            return f"{hours}h"
        if SYSTEM_LANGUAGE == "es":
            return "queda 1 hora" if hours == 1 else f"quedan {hours} horas"
        return "1 hour left" if hours == 1 else f"{hours} hours left"
    if short:
        return f"{days}d"
    if SYSTEM_LANGUAGE == "es":
        return "queda 1 día" if days == 1 else f"quedan {days} días"
    return "1 day left" if days == 1 else f"{days} days left"


def system_datetime(timestamp):
    if not timestamp:
        return t("no_data")
    date = dt.datetime.fromtimestamp(timestamp, tz=dt.datetime.now().astimezone().tzinfo)
    try:
        return date.strftime("%c")
    except Exception:
        return date.isoformat(timespec="minutes")


def pace(bucket, now=None):
    if not bucket or not bucket.get("reset") or not bucket.get("minutes"):
        return None

    now_ts = now or time.time()
    window_seconds = bucket["minutes"] * 60
    start_ts = bucket["reset"] - window_seconds
    elapsed_seconds = min(max(now_ts - start_ts, 0), window_seconds)
    expected_used = (elapsed_seconds / window_seconds) * 100 if window_seconds > 0 else 0
    diff = bucket["used"] - expected_used

    if diff >= 20:
        label = t("very_high")
        severity = "critical"
    elif diff >= 8:
        label = t("high")
        severity = "warning"
    elif diff <= -8:
        label = t("quiet")
        severity = "ok"
    else:
        label = t("normal")
        severity = "ok"

    return {
        "expected_used": expected_used,
        "diff": diff,
        "label": label,
        "severity": severity,
        "start": start_ts,
    }


def classify(five_hour, weekly):
    buckets = [b for b in (five_hour, weekly) if b]
    if not buckets:
        return "unknown", "Codex ?", "gray"

    lowest = min(b["remaining"] for b in buckets)
    paces = [p for p in (pace(five_hour), pace(weekly)) if p]
    worst_pace = "critical" if any(p["severity"] == "critical" for p in paces) else "warning" if any(p["severity"] == "warning" for p in paces) else "ok"

    five_text = format_title_bucket("5h", five_hour, with_marker=False)
    weekly_text = format_title_bucket(t("weekly_short"), weekly, with_marker=False)
    title = f"{five_text} / {weekly_text}".strip()

    if lowest <= 10:
        return "critical", title, None
    if lowest <= 25 or worst_pace == "warning":
        return "warning", title, None
    if worst_pace == "critical":
        return "critical", title, None
    return "ok", title, None


def format_title_bucket(label, bucket, with_marker=True):
    if not bucket:
        return f"{label} ?"
    marker = bucket_marker(bucket) if with_marker else ""
    return f"{marker}{label} {bucket['remaining']:.0f}% {time_left_text(bucket['reset'], short=True)}"


def bucket_marker(bucket):
    color = bucket_color(bucket)
    if color == "red":
        return "🔴 "
    if color == "orange":
        return "🟠 "
    if color == "green":
        return "🟢 "
    return ""


def bucket_color(bucket):
    p = pace(bucket)
    if bucket["remaining"] <= 10 or (p and p["severity"] == "critical"):
        return "red"
    if bucket["remaining"] <= 25 or (p and p["severity"] == "warning"):
        return "orange"
    return "green"


def blend_pixel(dst, src):
    sr, sg, sb, sa = src
    if sa <= 0:
        return dst
    if sa >= 255:
        return src
    dr, dg, db, da = dst
    alpha = sa / 255.0
    inv = 1.0 - alpha
    return (
        int(sr * alpha + dr * inv),
        int(sg * alpha + dg * inv),
        int(sb * alpha + db * inv),
        max(da, sa),
    )


def set_pixel(img, width, height, x, y, color):
    if 0 <= x < width and 0 <= y < height:
        idx = y * width + x
        img[idx] = blend_pixel(img[idx], color)


def draw_circle(img, width, height, cx, cy, radius, color):
    min_x = math.floor(cx - radius)
    max_x = math.ceil(cx + radius)
    min_y = math.floor(cy - radius)
    max_y = math.ceil(cy + radius)
    for y in range(min_y, max_y + 1):
        for x in range(min_x, max_x + 1):
            dx = x - cx
            dy = y - cy
            distance = math.sqrt(dx * dx + dy * dy)
            if distance <= radius:
                edge = max(0.0, min(1.0, radius - distance))
                alpha = int(color[3] * min(1.0, edge + 0.35))
                set_pixel(img, width, height, x, y, (*color[:3], alpha))


def draw_line(img, width, height, x1, y1, x2, y2, thickness, color):
    steps = max(1, int(math.hypot(x2 - x1, y2 - y1) * 2))
    for i in range(steps + 1):
        t = i / steps
        x = x1 + (x2 - x1) * t
        y = y1 + (y2 - y1) * t
        draw_circle(img, width, height, x, y, thickness / 2, color)


def draw_codex_mark(img, width, height, cx, cy, color):
    points = []
    radius = 16
    for i in range(6):
        angle = -math.pi / 2 + i * math.pi / 3
        points.append((cx + math.cos(angle) * radius, cy + math.sin(angle) * radius))

    for i in range(6):
        x1, y1 = points[i]
        x2, y2 = points[(i + 1) % 6]
        draw_line(img, width, height, x1, y1, x2, y2, 4, color)

    for x, y in points:
        draw_circle(img, width, height, x, y, 3.2, color)
    draw_circle(img, width, height, cx, cy, 3.5, color)


def downsample(img, width, height, scale):
    out_w = width // scale
    out_h = height // scale
    out = []
    for y in range(out_h):
        for x in range(out_w):
            pixels = []
            for yy in range(scale):
                for xx in range(scale):
                    pixels.append(img[(y * scale + yy) * width + (x * scale + xx)])
            out.append(tuple(sum(p[i] for p in pixels) // len(pixels) for i in range(4)))
    return out, out_w, out_h


def png_base64(pixels, width, height):
    raw = bytearray()
    for y in range(height):
        raw.append(0)
        for x in range(width):
            raw.extend(pixels[y * width + x])

    def chunk(kind, data):
        body = kind + data
        return struct.pack(">I", len(data)) + body + struct.pack(">I", zlib.crc32(body) & 0xFFFFFFFF)

    png = (
        b"\x89PNG\r\n\x1a\n"
        + chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 6, 0, 0, 0))
        + chunk(b"IDAT", zlib.compress(bytes(raw), 9))
        + chunk(b"IEND", b"")
    )
    return base64.b64encode(png).decode("ascii")


def status_icon(five_hour, weekly, mode="combined"):
    scale = 3
    width = (22 if mode in {"five", "weekly"} else 42) * scale
    height = 18 * scale
    img = [(0, 0, 0, 0)] * (width * height)

    five_color = COLORS.get(bucket_color(five_hour) if five_hour else "gray", COLORS["gray"])
    weekly_color = COLORS.get(bucket_color(weekly) if weekly else "gray", COLORS["gray"])

    if mode == "five":
        draw_codex_mark(img, width, height, 11 * scale, 9 * scale, five_color)
    elif mode == "weekly":
        draw_codex_mark(img, width, height, 11 * scale, 9 * scale, weekly_color)
    else:
        draw_codex_mark(img, width, height, 11 * scale, 9 * scale, five_color)
        draw_codex_mark(img, width, height, 31 * scale, 9 * scale, weekly_color)

    small, small_w, small_h = downsample(img, width, height, scale)
    return png_base64(small, small_w, small_h)


def format_reset(timestamp):
    if not timestamp:
        return t("no_data")
    date = dt.datetime.fromtimestamp(timestamp, tz=dt.datetime.now().astimezone().tzinfo)
    now = local_now()
    if date.date() == now.date():
        return date.strftime("hoy %H:%M")
    return date.strftime("%d %b %H:%M")


def format_full_reset(timestamp):
    if not timestamp:
        return t("no_data")
    return system_datetime(timestamp)


def bar(remaining):
    total = 12
    filled = round((remaining / 100) * total)
    return "[" + ("#" * filled) + ("-" * (total - filled)) + "]"


def read_state():
    try:
        return json.loads(STATE_PATH.read_text(encoding="utf-8"))
    except Exception:
        return {}


def write_state(state):
    try:
        STATE_PATH.write_text(json.dumps(state, indent=2), encoding="utf-8")
    except Exception:
        pass


def configured_display_mode():
    explicit = os.environ.get("CODEX_LIMITS_DISPLAY", "").strip().lower()
    if explicit in {"separate", "five", "weekly", "combined"}:
        return explicit

    mode = str(read_state().get("display_mode", "combined")).strip().lower()
    return mode if mode in {"separate", "five", "weekly", "combined"} else "separate"


def handle_action():
    if len(sys.argv) < 3 or sys.argv[1] != "set-mode":
        return False

    mode = sys.argv[2].strip().lower()
    if mode not in {"separate", "five", "weekly", "combined"}:
        return True

    state = read_state()
    state["display_mode"] = mode
    write_state(state)
    try:
        subprocess.run(["open", "-g", "swiftbar://refreshallplugins"], timeout=5, check=False)
    except Exception:
        pass
    return True


def notify_if_needed(severity, five_hour, weekly):
    if severity not in {"warning", "critical"}:
        return

    now = time.time()
    state = read_state()
    key = {
        "severity": severity,
        "five_reset": five_hour.get("reset") if five_hour else None,
        "weekly_reset": weekly.get("reset") if weekly else None,
        "five_remaining": round(five_hour.get("remaining", 100)) if five_hour else None,
        "weekly_remaining": round(weekly.get("remaining", 100)) if weekly else None,
        "five_pace": pace(five_hour).get("label") if pace(five_hour) else None,
        "weekly_pace": pace(weekly).get("label") if pace(weekly) else None,
    }
    key_text = json.dumps(key, sort_keys=True)

    last_key = state.get("last_notification_key")
    last_at = float(state.get("last_notification_at", 0))
    if last_key == key_text and now - last_at < 30 * 60:
        return

    title = t("critical_limit") if severity == "critical" else t("attention")
    parts = []
    if five_hour:
        p = pace(five_hour)
        extra = t("pace_extra", pace=p["label"]) if p and p["label"] != t("normal") else ""
        parts.append(f"5h: {five_hour['remaining']:.0f}% {t('remaining')}, {time_left_text(five_hour['reset'])}{extra}")
    if weekly:
        p = pace(weekly)
        extra = t("pace_extra", pace=p["label"]) if p and p["label"] != t("normal") else ""
        parts.append(f"{t('weekly_short')}: {weekly['remaining']:.0f}% {t('remaining')}, {time_left_text(weekly['reset'])}{extra}")
    message = " | ".join(parts)

    script = f'display notification {json.dumps(message)} with title {json.dumps(title)}'
    try:
        subprocess.run(["osascript", "-e", script], timeout=5, check=False)
        state["last_notification_key"] = key_text
        state["last_notification_at"] = now
        write_state(state)
    except Exception:
        pass


def print_bucket_section(label, bucket):
    header_style = "color=white font=Helvetica-Bold"
    if not bucket:
        print(f"{label}: {t('no_data')} | {header_style}")
        return
    color = "red" if bucket["remaining"] <= 10 else "orange" if bucket["remaining"] <= 25 else "green"
    print(f"{label} | {header_style}")
    print(f"{bucket['remaining']:.0f}% {t('remaining')} {bar(bucket['remaining'])} | color={color}")
    print(f"{t('used')}: {bucket['used']:.0f}%")
    print(f"{t('time_left')}: {time_left_text(bucket['reset'])}")
    print(f"{t('reset')}: {format_full_reset(bucket['reset'])}")
    p = pace(bucket)
    if p:
        pace_color = "red" if p["severity"] == "critical" else "orange" if p["severity"] == "warning" else "green"
        sign = "+" if p["diff"] >= 0 else ""
        print(
            f"{t('pace')}: {p['label']} ("
            f"{t('used_vs', used=bucket['used'], expected=p['expected_used'], sign=sign, diff=p['diff'])}) | color={pace_color}"
        )
        print(f"{t('estimated_start')}: {format_full_reset(p['start'])}")


def plugin_mode():
    explicit = os.environ.get("CODEX_LIMITS_MODE", "").strip().lower()
    if explicit in {"five", "weekly", "combined"}:
        return explicit

    display_mode = configured_display_mode()
    slot = os.environ.get("CODEX_LIMITS_SLOT", "").strip().lower()
    if not slot:
        name = Path(sys.argv[0]).name.lower()
        slot = "weekly" if "weekly" in name or "sem" in name else "primary"

    if slot == "weekly":
        return "weekly" if display_mode == "separate" else "hidden"

    if display_mode == "separate":
        return "five"
    return display_mode


def print_settings_section():
    current = configured_display_mode()
    options = [
        ("separate", t("show_separate")),
        ("combined", t("show_combined")),
        ("five", t("only_five")),
        ("weekly", t("only_weekly")),
    ]

    print("---")
    print(f"{t('settings')} | color=white font=Helvetica-Bold")
    print(t("display_mode"))
    script = str(Path(__file__).resolve()).replace('"', '\\"')
    for mode, label in options:
        mark = "✓ " if mode == current else ""
        print(f'{mark}{label} | bash="{script}" param1=set-mode param2={mode} terminal=false refresh=true')


def title_for_mode(mode, five_hour, weekly):
    if mode == "weekly":
        return format_title_bucket(t("weekly_short"), weekly, with_marker=False), status_icon(five_hour, weekly, "weekly")
    if mode == "combined":
        _, title, _ = classify(five_hour, weekly)
        return title, status_icon(five_hour, weekly, "combined")
    return format_title_bucket("5h", five_hour, with_marker=False), status_icon(five_hour, weekly, "five")


def main():
    mode = plugin_mode()
    if mode == "hidden":
        return 0

    rate_limits, timestamp, source_file = find_latest_rate_limits()
    if not rate_limits:
        print(f"{t('codex_no_data')} | color=gray")
        print("---")
        print(t("reviewed_folder"))
        print(f"{t('folder_checked')}: {SESSIONS_DIR}")
        print_settings_section()
        print("---")
        print(f"{t('open_codex_usage')} | href={USAGE_URL}")
        return 0

    primary = normalize_bucket(rate_limits.get("primary") or rate_limits.get("primary_window"))
    secondary = normalize_bucket(rate_limits.get("secondary") or rate_limits.get("secondary_window"))

    buckets = [b for b in (primary, secondary) if b]
    five_hour = None
    weekly = None
    if buckets:
        five_hour_candidates = [b for b in buckets if b["minutes"] and b["minutes"] < 1440]
        weekly_candidates = [b for b in buckets if b["minutes"] and b["minutes"] >= 1440]
        five_hour = min(five_hour_candidates, key=lambda b: abs(b["minutes"] - 300), default=None)
        weekly = min(weekly_candidates, key=lambda b: abs(b["minutes"] - 10080), default=None)

    severity, _, _ = classify(five_hour, weekly)
    if mode != "five":
        notify_if_needed(severity, five_hour, weekly)

    title, icon = title_for_mode(mode, five_hour, weekly)
    print(f"{title} | image={icon}")
    print("---")
    print_bucket_section(t("five_hour_limit"), five_hour)
    print("---")
    print_bucket_section(t("weekly_limit"), weekly)

    credits = rate_limits.get("credits")
    plan = rate_limits.get("plan_type") or t("unknown")
    print("---")
    print(f"{t('plan')}: {plan}")
    if isinstance(credits, dict):
        if credits.get("unlimited"):
            print(f"{t('credits')}: {t('credits_unlimited')}")
        elif credits.get("balance") is not None:
            print(f"{t('credits')}: {credits.get('balance')}")
        elif credits.get("has_credits") is False:
            print(f"{t('credits')}: {t('credits_no_extra')}")

    if timestamp:
        age = int(max(0, time.time() - timestamp) / 60)
        print(t("data_updated", minutes=age))
    if source_file:
        print(f"{t('source')}: {source_file.name}")

    print_settings_section()
    print("---")
    print(f"{t('official_usage')} | href={USAGE_URL}")
    print(f"{t('pricing')} | href={PRICING_URL}")
    print(f"{t('refresh_swiftbar')} | refresh=true")
    return 0


if __name__ == "__main__":
    try:
        if handle_action():
            raise SystemExit(0)
        raise SystemExit(main())
    except Exception as exc:
        print(f"{t('codex_error')} | color=red")
        print("---")
        print(str(exc).replace("|", "-"))
        raise SystemExit(1)
