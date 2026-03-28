#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# <xbar.title>Claude Usage</xbar.title>
# <xbar.version>v2.1</xbar.version>
# <xbar.author>aggel008</xbar.author>
# <xbar.author.github>aggel008</xbar.author.github>
# <xbar.desc>Live Claude.ai session (5h) and weekly usage in your menu bar — no API keys, reads directly from your browser</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/aggel008/claude-usage-bar/main/screenshot.png</xbar.image>
# <xbar.dependencies>python3</xbar.dependencies>
# <xbar.abouturl>https://github.com/aggel008/claude-usage-bar</xbar.abouturl>
# <swiftbar.title>Claude Usage</swiftbar.title>
# <swiftbar.version>2.1</swiftbar.version>
# <swiftbar.author>aggel008</swiftbar.author>
# <swiftbar.author.github>aggel008</swiftbar.author.github>
# <swiftbar.desc>Live Claude.ai session (5h) and weekly usage in your menu bar — no API keys, reads directly from your browser</swiftbar.desc>
# <swiftbar.image>https://raw.githubusercontent.com/aggel008/claude-usage-bar/main/screenshot.png</swiftbar.image>
# <swiftbar.dependencies>python3</swiftbar.dependencies>
# <swiftbar.refreshTime>5</swiftbar.refreshTime>
# <swiftbar.hideAbout>true</swiftbar.hideAbout>
# <swiftbar.hideRunInTerminal>true</swiftbar.hideRunInTerminal>
# <swiftbar.hideLastUpdated>true</swiftbar.hideLastUpdated>
# <swiftbar.hideDisablePlugin>true</swiftbar.hideDisablePlugin>

import json, subprocess, sys, os
from datetime import datetime, timezone

CONFIG = os.path.expanduser('~/.claude-usage.conf')
BLUE   = '#5C8EFF'
DIM    = '#888888'
RED    = '#FF5555'
WHITE  = '#E8E8E8'
YELLOW = '#F5A623'

def load_config():
    cfg = {}
    if not os.path.exists(CONFIG):
        return cfg
    with open(CONFIG) as f:
        for line in f:
            line = line.strip()
            if '=' in line and not line.startswith('#'):
                k, v = line.split('=', 1)
                cfg[k.strip()] = v.strip()
    return cfg

def save_org(org_id):
    cfg = load_config()
    cfg['ORG_ID'] = org_id
    with open(CONFIG, 'w') as f:
        for k, v in cfg.items():
            f.write(f'{k}={v}\n')

def js_in_browser(js, app):
    script = f"""
tell application "{app}"
    repeat with w in windows
        repeat with t in tabs of w
            if URL of t contains "claude.ai" then
                return execute t javascript "{js}"
            end if
        end repeat
    end repeat
    return "NO_TAB"
end tell
"""
    r = subprocess.run(['osascript', '-e', script], capture_output=True, text=True, timeout=10)
    if r.returncode != 0:
        raise RuntimeError(r.stderr.strip())
    return r.stdout.strip()

def xhr(path):
    js = (
        "var x=new XMLHttpRequest();"
        f"x.open('GET','{path}',false);"
        "x.setRequestHeader('Accept','application/json');"
        "x.send();"
        "x.status+'|||'+x.responseText"
    )
    # Try supported browsers in order
    for app in ['Comet', 'Google Chrome', 'Chromium', 'Brave Browser', 'Microsoft Edge']:
        try:
            result = js_in_browser(js, app)
            if result != 'NO_TAB':
                status, _, body = result.partition('|||')
                if status.strip() != '200':
                    raise RuntimeError(f'HTTP {status.strip()}')
                return json.loads(body)
        except RuntimeError:
            continue
        except Exception:
            continue
    raise RuntimeError('Open claude.ai in Chrome, Brave, Edge, or Comet')

def until(iso):
    try:
        dt = datetime.fromisoformat(iso.replace('Z', '+00:00'))
        d  = dt - datetime.now(timezone.utc)
        s  = int(d.total_seconds())
        if s <= 0: return 'soon'
        h, m = s // 3600, (s % 3600) // 60
        return f'{h}h {m}m' if h else f'{m}m'
    except:
        return '?'

def bar(pct, width=22):
    if pct is None:
        return '░' * width, DIM
    n = round(width * max(0, min(100, pct)) / 100)
    color = BLUE
    if pct >= 90: color = RED
    elif pct >= 70: color = YELLOW
    return '█' * n + '░' * (width - n), color

# ── main ──────────────────────────────────────────────────────────────────────
cfg    = load_config()
org_id = cfg.get('ORG_ID')

try:
    if not org_id:
        boot   = xhr('/api/bootstrap')
        org_id = boot['account']['memberships'][0]['organization']['uuid']
        save_org(org_id)

    data = xhr(f'/api/organizations/{org_id}/usage')

    s_pct   = data.get('five_hour',  {}).get('utilization')
    s_reset = data.get('five_hour',  {}).get('resets_at')
    w_pct   = data.get('seven_day',  {}).get('utilization')
    w_reset = data.get('seven_day',  {}).get('resets_at')

except RuntimeError as e:
    print('✦')
    print('---')
    print(f'{e} | color={RED}')
    sys.exit(0)
except Exception as e:
    print('✦ !')
    print('---')
    print(f'Error: {str(e)[:80]} | color={RED}')
    sys.exit(0)

# ── menu bar icon ──────────────────────────────────────────────────────────────
if s_pct is None:
    print('✦')
elif s_pct >= 90:
    print(f'✦ {int(s_pct)}% | color={RED}')
elif s_pct >= 70:
    print(f'✦ {int(s_pct)}% | color={YELLOW}')
else:
    print(f'✦ {int(s_pct)}%')

print('---')

# ── session ────────────────────────────────────────────────────────────────────
s_bar, s_color = bar(s_pct)
s_str  = f'{int(s_pct)}%' if s_pct is not None else '?'
s_time = f'↺ {until(s_reset)}' if s_reset else ''
print(f'SESSION  {s_str}   {s_time} | color={WHITE} font=Menlo size=11')
print(f'{s_bar} | color={s_color} font=Menlo size=10')

print('---')

# ── weekly ─────────────────────────────────────────────────────────────────────
w_bar, w_color = bar(w_pct)
w_str  = f'{int(w_pct)}%' if w_pct is not None else '?'
w_time = f'↺ {until(w_reset)}' if w_reset else ''
print(f'WEEKLY   {w_str}   {w_time} | color={WHITE} font=Menlo size=11')
print(f'{w_bar} | color={w_color} font=Menlo size=10')

print('---')
print(f'Refresh | refresh=true color={BLUE}')
