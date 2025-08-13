#!/usr/bin/env python3.11
# Remember to change the Python call according to your system ⬆️

# <xbar.title>qBittorrent Monitor</xbar.title>
# <xbar.version>v1.3</xbar.version>
# <xbar.author>Julien Minniti</xbar.author>
# <xbar.author.github>Jumitti</xbar.author.github>
# <xbar.desc>Displays active torrents with remaining time.</xbar.desc>
# <xbar.image>https://github.com/Jumitti/qBittorrent_status_xbar/blob/main/qbt_status.png?raw=true</xbar.image>
# <xbar.abouturl>https://github.com/Jumitti/qBittorrent_status_xbar</xbar.abouturl>

# <xbar.dependencies>python,qbittorrent-api</xbar.dependencies>
# Use "pip install qbittorrent-api" in Terminal ⬆️

# <xbar.refresh>1s</xbar.refresh>

# <xbar.var>string(HOST="localhost"): Host</xbar.var>
# <xbar.var>number(PORT=8080): Port</xbar.var>
# <xbar.var>string(USERNAME="admin"): Username</xbar.var>
# <xbar.var>string(PASSWORD="123456"): Password</xbar.var>

import os
import qbittorrentapi
import datetime

HOST = os.getenv("HOST", "localhost")
PORT = int(os.getenv("PORT", 8080))
USERNAME = os.getenv("USERNAME", "admin")
PASSWORD = os.getenv("PASSWORD", "123456")

def format_speed(speed_bytes_per_s):
    """Convertit la vitesse en KB/s ou MB/s selon le seuil de 500 KB/s"""
    kb_s = speed_bytes_per_s / 1024
    if kb_s > 500:
        mb_s = kb_s / 1024
        return f"{mb_s:.2f} MB/s"
    else:
        return f"{kb_s:.1f} KB/s"

try:
    client = qbittorrentapi.Client(
        host=f"{HOST}:{PORT}",
        username=USERNAME,
        password=PASSWORD
    )
    client.auth_log_in()
    torrents = client.torrents_info()

    if not torrents:
        print("⚪ qBittorrent | color=gray")
        print("---")
        print("No torrent found")
    else:
        print(f"🌀 {len(torrents)} torrents")
        print("---")
        for t in torrents:
            progress = f"{t.progress*100:.1f}%"
            dlspeed = format_speed(t.dlspeed)
            upspeed = format_speed(t.upspeed)
            remaining = t.eta
            remaining_str = "∞" if remaining < 0 else str(datetime.timedelta(seconds=remaining))
            print(f"{t.name} | color=black")
            print(f"--📈 {progress} | color=blue")
            print(f"--⬇ {dlspeed} ⬆ {upspeed} | color=gray")
            print(f"--⏳ {remaining_str} | color=gray")

except qbittorrentapi.LoginFailed:
    print("❌ Connection failed | color=red")
except Exception as e:
    print(f"⚠️ Error: {str(e)} | color=red")
