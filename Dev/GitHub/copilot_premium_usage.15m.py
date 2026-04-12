#!/usr/bin/env python3

# <xbar.title>Copilot Premium Requests</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>David Fernández</xbar.author>
# <xbar.author.github>driade</xbar.author.github>
# <xbar.desc>Shows GitHub Copilot premium interaction quota usage.</xbar.desc>
# <xbar.dependencies>python3</xbar.dependencies>
# <xbar.abouturl>https://github.com/driade/xbar-copilot-premium-usage</xbar.abouturl>

import json
import urllib.request
import urllib.error

API_URL = "https://api.github.com/copilot_internal/user"

# Paste your GitHub token here from https://github.com/settings/tokens > Generate new token (classic)
# SECURITY: Do not commit this file with a real token.
GITHUB_TOKEN_INLINE = ""


def get_token():
    if isinstance(GITHUB_TOKEN_INLINE, str) and GITHUB_TOKEN_INLINE.strip():
        return GITHUB_TOKEN_INLINE.strip()
    return None


def fetch_quota(token):
    req = urllib.request.Request(
        API_URL,
        headers={
            "Authorization": f"Bearer {token}",
            "Accept": "application/json",
        },
    )
    with urllib.request.urlopen(req, timeout=15) as resp:
        return json.load(resp)


def parse_quota(data):
    """Returns (used, total, remaining) or None if unlimited."""
    snapshots = data.get("quota_snapshots", {})
    premium = snapshots.get("premium_interactions", {})

    if premium.get("unlimited"):
        return None

    total = premium.get("entitlement")
    remaining = premium.get("remaining") or premium.get("quota_remaining")

    if total is None or remaining is None:
        return None

    used = max(total - remaining, 0)
    return used, total, remaining


def main():
    token = get_token()
    if not token:
        print("Copilot: ?")
        print("---")
        print("Missing token. Set GITHUB_TOKEN_INLINE in the script.")
        return 1

    try:
        data = fetch_quota(token)
    except urllib.error.HTTPError as e:
        print("Copilot: ?")
        print("---")
        print(f"HTTP Error: {e.code}")
        return 1
    except Exception as e:
        print("Copilot: ?")
        print("---")
        print(f"Error: {e}")
        return 1

    quota = parse_quota(data)

    if quota is None:
        print("Copilot: ∞")
        return 0

    used, total, remaining = quota
    pct = int(used / total * 100) if total > 0 else 0

    print(f"{used}/{total} - {pct}%")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
