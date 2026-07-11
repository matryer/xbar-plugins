#!/usr/bin/python3
# -*- coding: utf-8 -*-
# <xbar.title>Claude Usage Monitor</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>David Chou</xbar.author>
# <xbar.author.github>davidchou821</xbar.author.github>
# <xbar.desc>Monitor your Claude subscription usage in the menu bar: 5-hour window & weekly limits (shared across claude.ai web / desktop / Claude Code), local token & API-equivalent cost stats, multi-account switching, and a system notification at 90%. Reads Claude Code's OAuth token from Keychain (macOS asks for permission); token is only sent to api.anthropic.com. Designed for SwiftBar.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/davidchou821/claude-usage-widget/main/docs/screenshot.png</xbar.image>
# <xbar.dependencies>python3, Claude Code (logged in), ccusage (token/cost section)</xbar.dependencies>
# <xbar.abouturl>https://github.com/davidchou821/claude-usage-widget</xbar.abouturl>
#
# SwiftBar plugin: Claude 用量監測（方案額度 % ＋ token/費用統計）
# 資料來源：
#   1. 方案額度：Keychain 的 Claude Code OAuth token → api.anthropic.com/api/oauth/usage
#      可把不同登入帳號的 token 另存為 claude-usage-widget:<名稱> 的 Keychain 條目切換查詢
#   2. token/費用：ccusage（讀 ~/.claude/projects 本機紀錄，不分帳號，費用為 API 等值價非實際帳單）
import glob
import json
import os
import re
import shutil
import subprocess
import sys
import time
from datetime import datetime, timezone

PLUGIN = os.path.abspath(__file__)
STATE_FILE = os.path.expanduser("~/.config/claude-usage-widget/state.json")
KC_PREFIX = "claude-usage-widget:"  # 自存帳號的 Keychain service 前綴
CLIENT_ID = "9d1c250a-e61b-44d9-88ed-5944d1962f5e"  # Claude Code 的 OAuth client（續期 token 用）
NOTIFY_AT = 90  # 用量 ≥ 此 % 發系統通知，每個重置窗口只提醒一次


# SwiftBar 的 PATH 只有系統目錄，補上 homebrew／nvm 讓 ccusage 與它依賴的 node 找得到
_extra = ["/opt/homebrew/bin", "/usr/local/bin"] + sorted(
    glob.glob(os.path.expanduser("~/.nvm/versions/node/*/bin"))
)[-1:]
os.environ["PATH"] = os.pathsep.join(_extra + [os.environ.get("PATH", "")])


def find_ccusage():
    return shutil.which("ccusage") or "ccusage"


CCUSAGE = find_ccusage()
WEEKDAY = ["一", "二", "三", "四", "五", "六", "日"]


def sh(cmd, timeout=30, stdin=None):
    return subprocess.run(cmd, capture_output=True, text=True, timeout=timeout, input=stdin).stdout


def kc_read(service):
    return sh(["security", "find-generic-password", "-s", service, "-w"]).strip()


def kc_write(service, value):
    subprocess.run(
        ["security", "add-generic-password", "-U", "-s", service, "-a", "claude-usage-widget", "-w", value],
        capture_output=True,
    )


def kc_delete(service):
    subprocess.run(["security", "delete-generic-password", "-s", service], capture_output=True)


def load_state():
    try:
        with open(STATE_FILE) as f:
            return json.load(f)
    except Exception:
        return {}


def save_state(st):
    os.makedirs(os.path.dirname(STATE_FILE), exist_ok=True)
    with open(STATE_FILE, "w") as f:
        json.dump(st, f, ensure_ascii=False, indent=2)


def notify(title, msg):
    esc = lambda s: json.dumps(s, ensure_ascii=False)  # AppleScript 字串跳脫（不能轉 \uXXXX）
    subprocess.run(
        ["osascript", "-e", f"display notification {esc(msg)} with title {esc(title)}"],
        capture_output=True,
    )


def get_access_token(account):
    if not account:
        return json.loads(kc_read("Claude Code-credentials"))["claudeAiOauth"]["accessToken"]
    cred = json.loads(kc_read(KC_PREFIX + account))
    if cred.get("expiresAt", 0) / 1000 < time.time() + 60:  # 快過期就用 refresh token 續期
        try:
            # token 走 stdin（-d @-），避免出現在行程列表
            r = json.loads(sh([
                "curl", "-s", "-m", "15", "-X", "POST", "https://console.anthropic.com/v1/oauth/token",
                "-H", "Content-Type: application/json",
                "-d", "@-",
            ], stdin=json.dumps({
                "grant_type": "refresh_token",
                "refresh_token": cred["refreshToken"],
                "client_id": CLIENT_ID,
            })))
        except Exception:
            raise RuntimeError("token 續期失敗（網路問題？稍後會自動重試）")
        if "access_token" not in r:
            raise RuntimeError(f"帳號 {account} 的 token 已失效，請按住 ⌥ 移除後重新儲存")
        cred["accessToken"] = r["access_token"]
        cred["refreshToken"] = r.get("refresh_token", cred["refreshToken"])
        cred["expiresAt"] = int((time.time() + r.get("expires_in", 3600)) * 1000)
        kc_write(KC_PREFIX + account, json.dumps(cred))
    return cred["accessToken"]


# ---- 選單動作（點選單項目時 SwiftBar 會帶參數重跑本腳本）----
def handle_action(args, st):
    cmd = args[0]
    if cmd == "--switch":
        st["account"] = "" if args[1] == "__current__" else args[1]
        save_state(st)
    elif cmd == "--save":
        name = sh([
            "osascript", "-e",
            'text returned of (display dialog "為此帳號取個名字（例如 work、personal）："'
            ' default answer "" with title "Claude 用量 widget")',
        ]).strip()
        # 去掉會破壞 SwiftBar 選單格式的字元，長度以選單列寬度為限
        name = re.sub(r'[|"\'\\\s]+', "-", name).strip("-")[:20]
        if not name:
            sys.exit(0)  # 取消或空白
        try:
            cred = json.loads(kc_read("Claude Code-credentials"))["claudeAiOauth"]
        except Exception:
            notify("Claude 用量 widget", "讀不到 Claude Code 登入資訊，請先在 Claude Code 登入")
            sys.exit(0)
        kc_write(KC_PREFIX + name, json.dumps(cred))
        if name not in st.setdefault("accounts", []):
            st["accounts"].append(name)
        st["account"] = name
        save_state(st)
    elif cmd == "--remove":
        name = args[1]
        kc_delete(KC_PREFIX + name)
        st["accounts"] = [a for a in st.get("accounts", []) if a != name]
        if st.get("account") == name:
            st["account"] = ""
        save_state(st)
    sys.exit(0)


st = load_state()
if len(sys.argv) > 1:
    handle_action(sys.argv[1:], st)

account = st.get("account") or ""
if account and account not in st.get("accounts", []):
    account = ""  # 帳號已被移除的殘留設定


def color_for(pct):
    if not isinstance(pct, (int, float)):
        return ""
    if pct >= 85:
        return "red"
    if pct >= 60:
        return "orange"
    return ""


def fmt_reset(iso):
    try:
        dt = datetime.fromisoformat(iso).astimezone()
        now = datetime.now(timezone.utc).astimezone()
        prefix = "" if dt.date() == now.date() else f"週{WEEKDAY[dt.weekday()]} "
        return f"{prefix}{dt:%H:%M} 重置"
    except Exception:
        return ""


def fmt_tokens(n):
    if n >= 1_000_000:
        return f"{n/1_000_000:.1f}M"
    if n >= 1_000:
        return f"{n/1_000:.1f}K"
    return str(n)


# ---- 方案額度 ----
plan_lines, metrics, five, scoped = [], [], None, None
try:
    tok = get_access_token(account)
    # token 走 stdin（-H @-），避免出現在行程列表
    usage = json.loads(sh([
        "curl", "-s", "-m", "15", "https://api.anthropic.com/api/oauth/usage",
        "-H", "@-",
        "-H", "anthropic-beta: oauth-2025-04-20",
    ], stdin=f"Authorization: Bearer {tok}"))
    if usage.get("type") == "error":
        raise RuntimeError((usage.get("error") or {}).get("message", "API error"))
    fh = usage.get("five_hour") or {}
    sd = usage.get("seven_day") or {}
    five = fh.get("utilization")
    seven = sd.get("utilization")
    metrics.append(("5小時窗口", five, fh.get("resets_at", "")))
    metrics.append(("週限額（全部模型）", seven, sd.get("resets_at", "")))
    for lim in usage.get("limits", []):
        if lim.get("kind") == "weekly_scoped" and lim.get("scope"):
            scoped = lim.get("percent")
            name = (lim["scope"].get("model") or {}).get("display_name") or "模型"
            metrics.append((f"週限額（{name} 專屬）", scoped, lim.get("resets_at", "")))
    for label, pct, resets in metrics:
        pct_txt = f"{pct:.0f}%" if isinstance(pct, (int, float)) else "?%"
        reset_txt = fmt_reset(resets)
        suffix = f"（{reset_txt}）" if reset_txt else ""
        plan_lines.append(f"{label}：{pct_txt}{suffix} | color={color_for(pct)}")
except Exception as e:
    msg = str(e).replace("|", "/")[:80] or type(e).__name__
    plan_lines.append(f"方案額度讀取失敗：{msg} | color=gray")

# ---- 用量 ≥ NOTIFY_AT% 發系統通知（以 resets_at 當窗口 id，同一窗口不重發）----
notified = st.setdefault("notified", {})
dirty = False
for label, pct, resets in metrics:
    key = f"{account or '__current__'}:{label}"
    if isinstance(pct, (int, float)) and pct >= NOTIFY_AT and notified.get(key) != resets:
        who = f"「{account}」" if account else ""
        notify("Claude 用量提醒", f"{who}{label}已達 {pct:.0f}%（{fmt_reset(resets)}）")
        notified[key] = resets
        dirty = True
if dirty:
    save_state(st)

# ---- token / 費用（ccusage）----
cc_lines = []
today = datetime.now().strftime("%Y%m%d")
month_start = datetime.now().strftime("%Y%m01")
try:
    d = json.loads(sh([CCUSAGE, "daily", "--json", "--since", today], timeout=60))
    rows = d.get("daily", [])
    if rows:
        r = rows[-1]
        cc_lines.append(
            f"今日：輸出 {fmt_tokens(r.get('outputTokens', 0))}｜cache讀 {fmt_tokens(r.get('cacheReadTokens', 0))}｜${r.get('totalCost', 0):.2f}"
        )
        for mb in r.get("modelBreakdowns", []):
            cc_lines.append(
                f"--{mb.get('modelName','?')}: 輸出 {fmt_tokens(mb.get('outputTokens',0))}｜${mb.get('cost',0):.2f}"
            )
    m = json.loads(sh([CCUSAGE, "daily", "--json", "--since", month_start], timeout=60))
    mcost = sum(x.get("totalCost", 0) for x in m.get("daily", []))
    cc_lines.append(f"本月累計：${mcost:.2f}（API 等值價，非實際帳單）")
except Exception as e:
    cc_lines.append(f"ccusage 讀取失敗：{type(e).__name__} | color=gray")

# ---- 輸出 ----
worst = max([p for p in (five, scoped) if isinstance(p, (int, float))], default=None)
title_color = color_for(worst)
title_label = account or "CC"
if not isinstance(five, (int, float)):
    print(f"{title_label} ?% | sfimage=gauge.with.needle")
else:
    scoped_txt = f" ⁄{scoped:.0f}" if isinstance(scoped, (int, float)) else ""
    print(
        f"{title_label} {five:.0f}%{scoped_txt} | sfimage=gauge.with.needle"
        + (f" color={title_color}" if title_color else "")
    )
print("---")
print(f"方案額度（{account or '目前登入帳號'}）| size=12 color=gray")
for l in plan_lines:
    print(l)
print("---")
print("Token／費用（本機統計，不分帳號）| size=12 color=gray")
for l in cc_lines:
    print(l)
print("---")
print("帳號 | size=12 color=gray")
mark = "✓ " if not account else ""
print(f"{mark}目前登入（跟隨 Claude Code）| bash={PLUGIN} param1=--switch param2=__current__ terminal=false refresh=true")
for name in st.get("accounts", []):
    mark = "✓ " if name == account else ""
    print(f"{mark}{name} | bash={PLUGIN} param1=--switch param2={name} terminal=false refresh=true")
    print(f"移除「{name}」 | alternate=true color=red bash={PLUGIN} param1=--remove param2={name} terminal=false refresh=true")
print(f"把目前登入存為新帳號… | bash={PLUGIN} param1=--save terminal=false refresh=true")
print("---")
print("開 claude.ai 用量設定頁 | href=https://claude.ai/settings/usage")
print("立即更新 | refresh=true")
