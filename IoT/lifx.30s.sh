#!/bin/bash
# <xbar.title>LIFX</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Helen V. Holmes · helenvholmes.com</xbar.author>
# <xbar.author.github>helenvholmes</xbar.author.github>
# <xbar.desc>Control your LIFX lights from the menu bar — rooms, brightness, and color, with Lucide icons. Open source (MIT). Unofficial; not affiliated with LIFX.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/helenvholmes/lifx-mac-menubar/main/docs/menu.png</xbar.image>
# <xbar.dependencies>curl, jq</xbar.dependencies>
# <xbar.abouturl>https://github.com/helenvholmes/lifx-mac-menubar</xbar.abouturl>
# <swiftbar.hideRunInTerminal>true</swiftbar.hideRunInTerminal>
# <swiftbar.hideLastUpdated>true</swiftbar.hideLastUpdated>
# <swiftbar.hideDisablePlugin>true</swiftbar.hideDisablePlugin>
# <swiftbar.hideSwiftBar>true</swiftbar.hideSwiftBar>
#
# lifx-mac-menubar — https://github.com/helenvholmes/lifx-mac-menubar
# Copyright (c) 2026 Helen V. Holmes. MIT License (see LICENSE).
# Icons from Lucide (lucide.dev), ISC License (see LICENSE-LUCIDE).
# Unofficial project; not affiliated with or endorsed by LIFX.
#
# Setup: choose "Set LIFX token…" from the menu. To change it later,
# hold Option (⌥) while the menu is open and choose "Change LIFX token…".

export PATH="/usr/bin:/bin:/usr/sbin:/opt/homebrew/bin:/usr/local/bin:$PATH"
API="https://api.lifx.com/v1"
KEYCHAIN_SERVICE="lifx-token"
TOKEN=$(security find-generic-password -s "$KEYCHAIN_SERVICE" -w 2>/dev/null)

# Optional helper that shows notifications with a lightbulb icon (scripts/build-notifier.sh).
# Look for it next to the real script, following the symlink in the plugins folder.
SELF="${SWIFTBAR_PLUGIN_PATH:-$0}"
while [ -L "$SELF" ]; do
  link=$(readlink "$SELF"); case "$link" in /*) SELF="$link" ;; *) SELF="$(dirname "$SELF")/$link" ;; esac
done
NOTIFIER="$(cd "$(dirname "$SELF")" && pwd)/build/LIFX Menu Bar.app/Contents/MacOS/lifx-notifier"

notify() { # show a notification: with the helper's icon if it's built, otherwise through SwiftBar
  if [ -x "$NOTIFIER" ] && "$NOTIFIER" "LIFX" "$1"; then
    return
  elif [ -n "$SWIFTBAR" ] && command -v jq >/dev/null; then
    local plugin body
    plugin=$(basename "${SWIFTBAR_PLUGIN_PATH:-$0}"); plugin="${plugin%%.*}"
    body=$(printf '%s' "$1" | jq -sRr @uri)
    open -g "swiftbar://notify?plugin=$plugin&title=LIFX&body=$body"
  else
    local m="${1//\"/\'}"
    osascript -e "display notification \"$m\" with title \"LIFX\"" >/dev/null 2>&1
  fi
}

if ! command -v jq >/dev/null; then
  echo "| sfimage=exclamationmark.triangle"; echo "---"
  echo "This plugin needs jq — run: brew install jq"
  exit 0
fi

set_token() { # ask for a token, check it works, save it to the Keychain
  local tok
  tok=$(osascript -e 'text returned of (display dialog "Paste your LIFX personal access token.

You can create one at cloud.lifx.com → Settings → Personal access tokens." default answer "" with hidden answer with title "LIFX" buttons {"Cancel", "Save"} default button "Save")' 2>/dev/null) || exit 0
  tok=$(printf '%s' "$tok" | tr -d '[:space:]')
  [ -z "$tok" ] && exit 0
  if curl -s -m 10 -H "Authorization: Bearer $tok" "$API/lights/all" | jq -e 'type=="array"' >/dev/null 2>&1; then
    security add-generic-password -U -s "$KEYCHAIN_SERVICE" -a "$USER" -w "$tok"
    notify "Connected! Your lights will appear in a moment."
  else
    notify "That token didn't work. Check it at cloud.lifx.com and try again."
  fi
  exit 0
}
[ "$1" = "settoken" ] && set_token

setup_menu() { # $1 = message shown when there's no working token
  echo "| sfimage=lightbulb.slash"; echo "---"
  echo "$1"
  echo "---"
  echo "Set LIFX token… | bash=\"$0\" param1=settoken terminal=false refresh=true"
  echo "Get a token at cloud.lifx.com | href=https://cloud.lifx.com/settings"
  exit 0
}
[ -z "$TOKEN" ] && setup_menu "Connect your LIFX account to get started"

api() { curl -s -m 10 -H "Authorization: Bearer $TOKEN" "$@"; }

# ---------- Actions (called when you click a menu item) ----------
LOG="$HOME/Library/Logs/lifx-swiftbar.log"
run() { # send a command; alert + log if LIFX reports a problem
  local out code body
  out=$(curl -s -m 10 -w $'\n%{http_code}' -H "Authorization: Bearer $TOKEN" "$@")
  code="${out##*$'\n'}"; body="${out%$'\n'*}"
  echo "$(date '+%F %T') $code $* -> $body" | sed "s/$TOKEN/***/g" >> "$LOG"
  local msg=""
  if [ "$code" -ge 400 ] 2>/dev/null || [ -z "$code" ] || [ "$code" = "000" ]; then
    msg=$(echo "$body" | jq -r '.error // empty' 2>/dev/null); msg="${msg:-HTTP $code}"
  else
    msg=$(echo "$body" | jq -r '[.results[]? | select(.status != "ok") | "\(.label): \(.status)"] | join(", ")' 2>/dev/null)
  fi
  [ -n "$msg" ] && notify "$msg"
}

case "$1" in
  toggle)
    run -X POST "$API/lights/$2/toggle"
    sleep 1; exit 0 ;;
  set)
    sel="$2"; key="$3"; val="$4"
    if [ "$key" = "power" ]; then
      run -X PUT "$API/lights/$sel/state" -d "power=$val"
    else
      run -X PUT "$API/lights/$sel/state" -d "power=on" --data-urlencode "$key=$val"
    fi
    sleep 1; exit 0 ;;
esac

# ---------- Icons: Lucide (lucide.dev, ISC license) as 16pt template PNGs ----------
# Edit the SVGs in icons/ and run scripts/build-icons.sh to regenerate.
# >>> generated icons (scripts/build-icons.sh) — do not edit by hand
# shellcheck disable=SC2034  # read indirectly by ic()
{
I_bed="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAAA9UlEQVR4Xu2XOwoCMRRFjx9s3IILEHQxIugStHY/YitYuAp3IIKNpbgAKws/3DGRxygiOpNY5EBISJF78/ImLwOJxJ0ecASmsQIyB67AObSBqhPM98HwwtGIbqDutt40IVi4cQXouPHG5cgvKL+2wBJY5xfaOQElYtntBIzzBlYBhO3GZKIrEz4H9saRQq82NHMa+/lvex33yK3ZAPrWQAiUAzMj1A5tQNiLrhbDwBPR74FkIEXgXS1omZSdAIOCLwtbf4LWAn8lq/48cuBQ8O4+IdP0R5CvBWWiCIhM82++gotz5fsyeakV8lmup79+AaSZSHAD1410Y50aSZIAAAAASUVORK5CYII="
I_cooking_pot="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAABzklEQVR4Xu3Xz4tOURzH8RdGKZNJ2dnIipqmZCFKKJGdlfwTFlZKUyMrO1mzpbCibFiwEQuliKysSChlYUbGr75P59R157jPmTvzxOJ511089z7n8/2e74/zg3/Mmp72t2A/9mAzXuIaPvbUq+Yg7uMnfrWed9hVrdSDo/hWMNx05BXW9tCuIsTDWMz+Nk5hGpO42nDsWFLbgAM4hweYx5UqSwW2NQxcLnzf3fj+MKVpoRCtz+2xE5UeRegzXxozjCKMujiUjEVR7ytofsVjXKq0t4TI66dk5A3upZC2CzH/XsQTXMBhBulYNhGdCO2ZZPB7RwGGwae4g5N9DWaOd+QwzzDyG+04lwqt1wyD0kIUhbKpFa+cwwj/Xdxk8G4kRKtEy0TrRAtFK/We4Zhh5BqIij+CjZjCdjzDj2EClazDDF4zqLFYS6KWorMGnO9osb/1+krfh83RbRy1lFJwIw1+hIu1QkM4jb3pPydKKWiSQ3t9mOoyCK2s+wcj27trGTswjsA4AqUI5PU/1u/VImtV7S0v0qLxHjtWwYOd+JA0n9fozY5wYzpb48D6dPFY6W7XHn+LpdeArstpHE7jOLa1xusO3qYDbNym/j9+A4K9wiKkQbCRAAAAAElFTkSuQmCC"
I_flame="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAACSklEQVR4Xu2WvWsWQRDGf0YlENRga0j8KGy0UAMWBiMogv4FNoI22mggYqGFoJ02dmKRLmnsRAS/OhsRBEH8KqwSP0FESSzUqCjP6yxMjr3Xm70qkAeW27udeWZ2Znb2YAntsAPY3o6iHEeA3za2ldOU4RDwC/hj41IZTRmGgXlnXE48L6MqwyNnfMbN95TRxTDiDN4Edrn3+zGqMpx3BkeN4o77driMtjmuOmNrTU018dO+zwGbm9NBT0QY+OzkN9n8MXDO5quB28CGIG9jHHQRuOK0tBEZTsfydTQSTbEcmDZD34ABp9gH3HNOfAC2NiWO4HiXBtQL3HDrn9pGQn2+2utXAh/NyLOM51q/5pxQ31gR2WHCPmu36vfVXn/LDHytIVaq7jonzkQdWAa8dASnKgQPbe1dF+J1wBeTU1HKqcZQk0kVracKajfQD5x2a9f/w3jZyR5obB24UHHAO5PmPzKpqWKn4zmbc6CuEQ054amM4ixwFHiSWfN45V425mTrqlO7TDgJTECnKAeBp3bU3uQIK1ABJ2RroM4B5TxBRh/YiMJH8n0kBf58749adfC6uZ5RC1W7ikypkGLoCBmkoz8lcXyHzkUVgoovVfyJkOY/jDn9yQL9zpUqz+WEorE3QKKCTRHUpbU+oLsA/uIRoU5Et3RoTTtPxuX8sVLjCfrl9o1INTEObAFW2dBc7TrlPMlfbGs8QZFI6ch1xeo3hb31zqtQHlWY3RzRmmQa51y3XhRroHOxKOxqUtr9W+CFXcH6MV08+AudzbeYpIvl1AAAAABJRU5ErkJggg=="
I_grid="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAABSElEQVR4Xu2XPU7DQBSEvyCqQHqSjhMgSo7AAXIAuAEFXdKlpwQhbsBB6AInABoE1FAn0VjGGhmvN4tiGSkeKVIUv+zMzq7fD2w7egEDRsARsB8x6DB//hKJ+waegPeY4SK8A5YNfBbALbBXJ6Ipct+QRBTwIxgCb5D99gzMILMuhBPgIn94BTzUxMrZKWRHJjE64o9y/KnZfhY7K2Bs8foew7nFiyvDjv3LL1zdzv+KryouF9AKOgGdA607sBu4+koyMXhManwlLhvI/6GaIq5fiagV+BG8moJYbhdSakE5vuAK3QEVlvs1LPkpRqnxBVp/CzoBnQP/ygHvggZrvIKp8DULLs8D6tuVOtWUTvLVvY0qI6UWiFxNqSCOx5B6tcxNzAS+5nWIXNDQIBEaIjYtRGveAH0XEBrNDoDjDY9mc+CzbvfbiRW4NagLqB3VgAAAAABJRU5ErkJggg=="
I_house="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAAB/klEQVR4Xu2XvUsdQRTFf0YhxKhFEElhI1GxUEHSaGNhY5fCxs7SP8DGQkXBXrBS06ZSwYBY2AhWIZWmSCGKiI0gaCEKIn5ylpnHddndt7u+h4LvwIPdN/ecc3fmzt1ZeGVU5fQXbwgYcPwt4DfwmFMvExohMJSZ/ek/jZUV7cBBhLlPRGOKKQv6gXNjvgu0OUNd+yQUo9iSYhi4NiabQINxqAPWzfgNMFKKDFRsM8CDEV8CaiLEq4H50PLo/kPeRD4Cv4zgHTCeQmwUuDW8FeBTCt4zfAG2jcgV8CODyCBwYfh/gKa0/G/AniGfAN/Tkg26gWOjcwh0FNNpBc5Cld5cjJQAce0OkbY8IqGCs8EbEFT3SyENafltKo/IDqwG4oPWIKjqUkFavlXLo9Cs7BbpNG6q3PtSuTutZaNX8LL72T5xkrmyl0B4hsT5D+zHJG41C9yohpKEWWAibg3dEipmOlHFIEuX0pMnmQsqrin3jkiFLDOgaffVOwf8DTn0AmMupsu9NYsiSwJ2zWW+mqCeegdlWYKyoJJAZQbe1Azo0OFRX4aSt5oFL9sH/rlWqmYz6RK4NIn0xVx7JI3LXB3St2u9kiPxM+HMH/4QyXu/EGcufIYgCXsKzmsU5klzEai1CcR9G34FeiJORC2OfBTzFHHjWvMd4DSG947xBK0kpe1ggwtFAAAAAElFTkSuQmCC"
I_key="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAACF0lEQVR4Xu2Wz0vVQRTFP4kUWBQGGrVoZz8EKcFV2NZt7lpkC3Ep0aJ1YODWcFX9F6mgRRi0ctEi2imKkEZR4kIpKKgszmNGTlNf33zl63OhB4b3+M7ce+69c+fMwAHCUWA0jOZG5y3ySeB3GH2NDCAl/wCcaVQAIn9nmX8COhtJ/tTIVf77+0muAH4BQ3sdhMrumavsylzkMYibexVECzBXsOfKPAaxBhypOghlvmDk60nDaf6NzZ+sMgA5n0ga7jNwMZBofsrmVaXKkJJ/NyKd+a5EhPStI4f9OvAklPUr1MY88Bjotcw8czXclYTwZxJQrEohzgIzSTmjhPqv1jwraLhU/WT30bakEJeA9wm5lGw6DFc1D0ZlvZB41YVzD3gOPALO1St7K7Bk5K+t1A5tzVtb9wPoqec8Bw/Nqfb12A5Gmlu09WM5BDvhNGx37wpwPMPhCdsudb4quGsMWDZ3Sni5a3a3Sthtoyn8u2rGarhc+Fr3kY0YQLtZqKNzoRMTsatHRgzgizk6lcsOf63dLGH3zxYsm/G1Eo6iIgruozSkzVFYXpWwfml2qRCVxqw5G86w1mmJQb/IWF8X3aYFejiMFIiRROiBPS6+hQuoEtw2x8puFWoqNxiG1NLvCgUqDakU/VDr6P/dgP5tA7hRKbNBujAOtVdNGojufFWirQryeg9D6cRl4Hwg05boUbJVBfkhhD82/8N54b6VLwAAAABJRU5ErkJggg=="
I_lamp_ceiling="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAABuklEQVR4Xu2WzytFQRTHP37l54YSEmWl2AhZ2YkSC/4FdmKlLCkp/AnY8i8QZfFYWkkWUpKNn2GBpPzoXOe+xnPfvTPXe6Tep6Y7zZzzPWemuWcG/jGFwJw26f86/cC7NunHIj+W1ycVhq/Zd+InCWSEXAJ/vgNh1AJHxkn3T7zrVzREy5mFDAT3kxWtQPLSjMtvdQZUArfAdoCd+Lbq+KEmm0oPUAXcAY3Ag+02TBirn7Z1CmDG0Bm31ZGDeayOz0CNrWMA1cCTap0ABTZaw0bWyzYOEawYekNRxsKOOrwBLTYOETSDpyUHMhFl3GFkux5l7MCGodsV5rdmGPY6BIiiz9BdTWdcD7yo4QGk/UXjsq/aEqMhSGTRyHIkbpQQRg39b4WpDLhRg0ugJAsJFAPnGkOK25c3xFgGy67tXSExk7dhWxZWHIUX039MzgNFQHmIVx3QrfN7wGmKbRPQqWO7wEWI1iN4MZ0QcX97ZwM85YXsz7c7KVsiB/Vag8jt5q9WkP69zl0BpZaazkwZq3zV0irlVfr+6iedVR2QN4B5uaSe+KUsFLBABoEt8GqGtE1gwGEhOXIk+QAKoKYf6nGkcQAAAABJRU5ErkJggg=="
I_lamp_desk="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAACJUlEQVR4XtWWvUsdQRTFf35ECwNiYxAbMX70aS3SCRYhoKilBItgk/SmEcQ/QEynJOkCoqBFVLCTtLGwUyQ2foCVxIBg4gdHZuA6vN2d93Z56mne292Ze849M/fOwBNFF/AFGHoI/T3AEXADXAHj1RRhySWgqiJC8m3nQFVEvDS2i3AVaHD2axn07hpYAl4BNUUvy7wjEdEvR+4xa775ZTkBvgIjQEsRYgaN3cr0swvaD/wvIcAL0a++/wQ+5XVnMiBS5pZcWb914vZTRO0CL8p1pdTut1mK/HkQtBv4CGwAF4EgORcNBTo0AXbchrPrHZJb9AYbeAWoj2VX5pZ8DWgElo0g2Z6EkNxXTxRC2z25oFLzDswlRAvn/3Dio5A1WXV+7ET8LhExa34qYid/My5on3jEzk+EysTbm7Zmo2bcBxctN7kgS31r7UjxSh3unxu7DsWQCxMmMzWbNKjDSazq3B/R9qyoCM+AAxO4PSWK2qttSPpf1m5PQqwLthxzZ24R64LKcc+5oA6nI7owxLrQCgyU015j0Aa8AU5ddpfAe2AY6AOaYoJUAgXW5UNnfri57PMZMFYJQRYWMoitCImU9blg726yXbWsdyrDaeAv0Ax0wt0xrPufTrgpoA7YAl7nUmCgbHyG7zKCqvNp7HnGuEzUmhH2UqHM0/DHfUy7iEQh6WayGDW7AFgHsrIuhUrm3IN1YBOYcadaDLQHvscMfNS4BcgZ51ZjkyzqAAAAAElFTkSuQmCC"
I_lamp_floor="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAABTUlEQVR4Xu2XsUoDQRCGP0XRBwhaiIUgNlYBG0HbvIFFSGMpGAvfII21YBEICNb6ELYWQqwsEgKpRe3ViMrAHCwXb3dWVkTYD67Y239n/p273duDP2YmIv8c0AE2AmOGqnuPiG1iH/g0XqJNTt+YXEyKNim7TvJLT+QrR7eT0oE1sNVoFCvAm87MUtpb1U6A1VCm2ZAAOATmVXdm0HdVI6vmwKD3sgA86IwegUVDwKgxoQq0gCVN2gNeDAZegXPV1YCmb4yUyUfb6XwG9kriNW2PS/efnPYRcGEwPsVWxLoPbVAS61t8j0C20o+fOC8hMSq35dC3oA6sV5jYBo617xS4qdCNgLsEE5lC3oei9OV3w0xoFfw62UCuQK5ArkCuQK5ADMvAwHBIEY1ok9MwJC8+z6I1EToTulwDJ87PqRxmNlVwr+YE+TkV7f/gC4SdcYbR/CFGAAAAAElFTkSuQmCC"
I_lamp="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAABy0lEQVR4Xu2Xuy8EURSHP88EiUaE6BREgoqCoFDRIujFo1Z6NOIPQKGhJyFqq1GiUUioScSjIULiVSBn3Enurp29585ubLNfMpnJ3HN+59zHnHsH8kyRMn4JMAL0AzUOnwfgENgDvpT6GakEDoBvzysBVOQigWXPwHaiS9kmUAU8mQTugCaFYDNwb3zEVzRiM2P1fsFDZdHym/bw+8OZEfoA6jyEaoF343vu4ZdEn9WLrRgi4hOuh94Y/mxbAj0xBCRomIB3B+ohGHYRkGmIy6nR+AQa0okURyhPAeWmbT1udGDD+JYBE1qdUuA6R5+RFLFHo3UDQSJOhq25W3Vau1mz9Ibc5rBvHKSOt2gcHIiGaMl6Eu0kZLhT6TQvroB2c0XRaBouIy1+ES2x7XDYBZxkUftdG9axJoEua+G4BH3aZZsW7SSizgPVQDcE9yikfdY0rkDG3j0DR8BLBj1vRq2pkudYRBWif6OQQGEEfJgHbhVFSmzmfIQ1jCsCpxalMY1wur0gHQPWy12HcFgTBoEdh606Afto7eqZjISgOo5rR0D+CULCAC7kAJIz2oA3j3XwCrRqomt/TgVJYjLqcGkhX8EmcKFJIO/8ALyBvJQiA3+XAAAAAElFTkSuQmCC"
I_lightbulb_off="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAACMUlEQVR4XtWXT0gVURTGf2kiohTpQkEoAqFFi4qEJGwpCEWb2rQKEhfRLgjaBIHiOogIUgNBcGUougzDjS0L2hRECxctisg2GkQp3+PO4zDM3D/P6YkfDO8x9/z55pxzzz0XDhhHnP/TwCNgFXgV4NQOXAHOAWeAbeAb8B5YA3438k3TwC7wFxgrMSDHD4HvTlby+UdrD4CWVBI3nHMfiacex3kiikR/Kok7hsQ/4G7OQEbgJzABDAMngD5gEJgEtgzJj8DxKkkoBVeBHo/RU8CGIbEM9TqLRigSIXQBHwyJkZBCESwJ5fdFopELhsBKom4d47miy9dECG/MzuoICRfhUo5AUWH68Njony0T9O3Xk0ZJaVDTelawO8rwxSyo0RXCR+CH0ZiDWgRSSBwz+uqWyRBrOdXXL0Ly7lhyurKhXtEQ1p2RP8DFBBJDpqm9bcizw01TSF+hVhcxfeKd0bu+HwLCvDEmw50RJHQWKHUv9+tcaANeGxJPnFFfJETyMtBaBQGhG9h0JHagXuEx6agM900URo3VppHoBT65R/8tmkbCh8NJIhtKY6FjdiAgrINHA67avLbiPeB5rAMfzpvuVjSQZu802Ook1K/epZ6ipUghINmomjiaEBrN/Ro+Qyn47O4Ikhc08isdOsqFStKRgqhI/G9USmLGzAm2GFVsWiuDblxZYepXl6GG8MtzQ9KaD5aEaiOpCDPcBm4VXDgUjYUAgVmo3ayuAVMB2eZgD9UO57bxZA4xAAAAAElFTkSuQmCC"
I_lightbulb="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAAB3UlEQVR4Xu2Xv0scQRiGHyWgYhAJRgNCbDSNBCJJIUIaKyFBsEzlnyBYaB//gYBgZW9jYxfUkNhE0IC2goiERLFIUJsQ/BXeZQbm7nZnd2aPqHAPLBw7874z883M9+3BLdMUMX4LMAqMAL1G/wP4CnwG/kZ4FqINmAV+AzcZzy9gBmgt5BiAVrrtGbh6QrvA0wB/L93AoTO4IvAeeAk8NM8rYA44dfodAI/LTkJnRPtqV6jfXR5TTXbD6b9WdgITjtkWJOcgD/X55ujG8wQ+vhijS2AwwOg5JBpF7lOAroJHwIUx+Rhhsmq08ujM0jd7jAeAB6Y9ZhVWI49nMRPocURHERH46Whcrwp8EXAzmq5bKG7Yz0PFQsnHXr/lCIMVo78GnkToE2z2uwJeB5gMQ6LRAjYDdDW8daJwUjC99gHHju5NmQmIecdsB2j3GOqsqA7YrVssO7jQNVp3TD94TBeq0rC9xqVRUvpuzP8AHSmOOvW6OVq9ClFm8oll2lndWIqJ3tnQT8UO4kPJZM88aYklr71BKqEfpUNAf04s9yG5rnXnhZPdsj5K9V4ZUH0L4StG/4V7tQUNLCowqvHVB1Hv6lJ88jjz/ENSWxAx1WoSeAc1B1gRWQoa/S7wD8VXjvwrOGh7AAAAAElFTkSuQmCC"
I_palette="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAAC5UlEQVR4Xs2XSahOYRjHf6YoRHEzUywoJRcZYmFIsbW1UKYNa7JwCStDGVNKioWNEkKR2LAiG8qUMk8hZChj/9Pz6vF+55z3nO9+4l9f3Xve5zz/Z36fA/8YXWryS34KMA0YBAyD7Nlz4AVwHbgCfK2pN4lRwB7gMfAz8XsHHANmJrVWQH9gB/ClAnGeYReAiRV4cjEOuBMRP7RILIDsvLf9RgNzgM3Ateidz8CqukZI2Vun6BGwDOhWUdEs4HxkyH6rlyTkmSc/DvRJvDUGmJAjsxL45AzZkmJXzn3YdwFdEy91AN+NZEOOrDrmgzNiUZm+7U5QngfygcBp4BU0kKgFLwKXrB3zMN9aU4WqOlLtNECtFqpdOfdh3wg8MZIfUbhP2pnOT5V4t9M5tz5PTn0eWmlpJLDGGSCZ8e68zap/E2SRKkI/4LVx3I2FNM3kdQhRXO0DgLMWYkWjWRxwTmqi/ob+Cd7vbla7Q1HNaH4Enmw2hCKb6l4+0wIDVtsEvGlFG1pUHRYwwhswxB3cd38rNdOBwTWNUu8HSIfaVHjqng/1Og+60PS1gx5urH4EJtcwoqhm5EhIwV7p616iVJ5PAu4B34AlZlAVqNoX5gjOc89u+RQ8cwchHbrb39sA0WVztQpzCXoC6+xcs+SPmaGKDKHxlqs495n3qZGcIj/qOI7Ewr4Ns9y0CMr5YuCGI9dS44s+gx9EGkhltZGHucDlCouLyAsXFA2gkIblNSIgcu2AZaua2lBhb2hnv5SOBG4DvaxftRfoGk1BS+gMM+CcFa4gg17a9a4L60FKkbDNeXKiwi4gaOUSmcg7Dd1YikIIZ5WFRJNT8m+A4Z22ABgbrWSKRJiOeVjrDD7cCgOE2ZERWjZWFHSHRrYuHUVBE7BlUCR8OkSgFtVg0rBSkWprUtr+igGClG91hZb6KmpZCmKoRVWQWiiLjFC06l7bTX2cttvHqci0VGhq6qI6VHFu/F/4BTDKBTWmhdEwAAAAAElFTkSuQmCC"
I_power="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAACRklEQVR4Xu2XzYtPYRTHP8OI8hILDaaRvCQ7Mgtlo1hQYzkpqVlhYcFKVkpZKH8AWzVloUgkKxSlqdHMAsU0JWXEQilvMYO+17m303XvfZ77kpryrdt96vec73k/5/5gHqMXOG+Pzv8cB4Bf9ujcCAsaSf3BMifrz7XQxoBO8N+Aqgj0A6eAgRax3gCcBbbW5VgJvLQKv1ciPOy6QOci3LE7b4B1RReKIrAQuApsMYFXda13eGZnKb8BLInhOuo8ew6saBGBRcADx3cmZMBi4LUJfAU2VQjEpEDoAz4Yp96rPGc+BYdd0V0CpkMWR+AdcNHuSbkiXIrrZulcWdE4xEZAWAp8Me5HZRFQvvbZj0+AmQjvYvEZsm7a5dPgU6C+X26MD2OZayDlVJdlteXXqIolRYz3U8BPE9A5BM+5Nr3sDVCeUnwKsQGTwKDd0zkEz5np8ga8dwxrQmyGich7gudUZyTwNeBDVNX/TbHZCb4tI3lhraJoqFi6hKaqvqDEnTmeH0S3TONqYG+H2rcD24zvrivev6CiUmXL0jGgpyMjbrt9sD/EmU5DGXEidDkCh5zyqPmiUH0zoe8tU7EDkvaTM7PA7giDE4w4qzXDj8QKOgwBHx3P6bocF5ywPBgFNkaQ6OPjMiQLLf3fcCVCrhAngR+OSCm5CRwDdtrG1IBRqBW1a27rSbkK+lzbYt4DPM1FI/Ws6q39cLCp53loXqgOHudCmzdAHo8DxyFZ70E06fN0SK2HZKvJOM12fcrdh+QLeP7gNzZPlTW2b3ECAAAAAElFTkSuQmCC"
I_refresh_cw="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAACeElEQVR4Xu2XS0tXQRiHn67WJqIiKZIIBKGE7gR+AttEiQkKBX4Go2XQKhctKsggWrWJoFq0MLcti6JSQjeWC8tAiCLwRjd+w0y8HmbOcY7mph74w59zeW/z3g7866ypGYAdwG5gj39/EpgCpivkHfb3X9XR2wxcBl4DvxK/t0A/cCiiQNd++F/sfpJG4CawUKK4aNBP4CFwwEg9a97Xf8f6ilC0AY/AGRH4CjwB561CL3QULUA7uOPR0XYAp4A+4EaFnihdwJyx+h3QA2woEbbOG/GsEK27QHcsAinkuVU+AGzM8EIR6AVmjNIXSzVgJ/DJPHwhQ3GRI8DHSO6U5sAlc+a3gKuZBpz24Q4l/gbYCmw2cnRUUVRqIdvHM8NukzRVpuG6yjmKboSHlHB1uAOuDFNGzAOtKcEKl178UtP7ZaH6DVbfW5akDNaaZ0NfF8MZMnLQLAjzwGE74S4jSYNlpVH/Vy8QR/1MWWSAlCp5hAbOSiOlIeJ/WruNgJQe+4sGaHwH1JxWnfs+yRXlbautvQHcFFWVhTxw2CpQc1CTSDUQWa4mUwdN1i3+xccpAbYLpoyQF7lsAia8Y3JwnxVgk3C0IHkWeAp889dlVJ0GdR3Y62VouL1PeWBXphABZatGal0umiOVLHXbJNYAuzxoqdBykbNBK+y3jXJF80SVF9YArU9ao2wuPAdOQnqW+wF2zpy53pfyzpTy1FL6HTjvS0YLifbA48Ag8BkYAsb8UiolTX7EaikN2S7UXc+A2xEria7NwH7gQcWML1aNsv1a1ZkXqfpwOAhcAUZKesVLcCvdolIro5hYS/102h75NPvgj+c/WfwGk1fcLYIlvBEAAAAASUVORK5CYII="
I_sofa="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAAB3ElEQVR4Xu2Wuy+EQRTFfx4F8YgSodSg0PoDFAqikUhUREnhL6EloiTR02lJhKBQEPFI0HgUHo3EI2czI3c/8z3sfqvak2yymblzzvnu3Jm5UEUVYXQD68Ad8FXmTxzi6sqabIk/likaMv0QMlETcCW3E278EDiPca4vGnRzu8BNTFwPMODm1oDJtEz4tEu8NiF43GRJ/+MgjiMXexsNCgl0uKAz4DPNbQaI49TFdWYx8K+IZqDOqDfk6KTRcFmNIgP9wIEJHAWWgaYyjDQDK8CI4dgH+qKcrcB1zNFbijGQpQglHjqSOjFtlnfOBG4Ci8ClG1MRtQdMpBlQMWutDFw4zi2jM2s5V82E0iZMm7HhEgxojf/6Kbe+xYxJ86cG7D6/uuAXI+pN/QV2TYizoFmfwPhu5jZSlNPmLVcRkm66beA5RTgL3oCduMCkDChtQ8BMtGId9I7443Ti9jaKJyjs9X0pBoQ996sYkrbgX+ANaJ88dFQqBctd0PQ1oKfXn1VV9LF7Qj9ycqL7Xz2B7wsEe+0nXsXltmSh9VfwO9OqaGWiEoKWU1/ea4+ShWrCp11t1kJOWzBv2jdtR2KjIwNyrN4wL4hLnJlqSk+o3oGxvNQdlzjFXUUVRfgGPcLI6uDo5VAAAAAASUVORK5CYII="
I_strip="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAABP0lEQVR4Xu2Wu0pDQRCGPy9djFqZQrBV1MLoU2iVTgQRfBULn8A6hZ02PomXzoiNgm+gKRPlX3Zh3BxNYM+aqOeDhYVlLjszuzNQUfHfmYoCMAOsAavg9mXSAzrAA7j9AJvAHfCeed0AG7H1BeA5s2F7sRdgUU7Mek+OgBW/vwIuvwpTAkrpPtACloFD4Czoa5vbzyUYGUbd2JFNpr1EzUi+DdOSwKuRdTaDA2PjVziwBOyZgrU0ge2C8Km4d4HGqKG9MMURo89DT+g0OtjyL0Wfyk50duJlHgscCM9RNsefgsB3ESg7BZ8iMIoDZTNZKZiYZ9g1cdZ3mYt5o9jZDM3oFjj2h+fgirKwZyegZnRg5K+tLnn2k+34CQYjrSFBw0LugUQ3Xw+3j0cyFaVGMq0cI9m9H8n6CamsqPhjfAD0h58tHx9oTQAAAABJRU5ErkJggg=="
I_sun="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAAB40lEQVR4XtWXuUrFQBSGP8XO/SVUfAVbEWxcEdTGwmdQ0FIEGx9BC1G0EkEQRLC3VAu3p7BQG1f+e+fgGCc3k9xclw9CJpM5S+acOTOBf0wLsOoutX+cYeDdXWoXormQVJU2T9Zv56IeB0ohzYEB4AiYrmHlwXvnt5NIh3RJZzQSUHxfgfkUqZgklKx0SJd0RjPjCUrBbLTkJ5Ix47pLZy58709zSVaRTNYs1ly/m8ATMAesJxzoB0aAHtd/CxwC1964NeAZ2AL2CnxAkE5gF3jzaoDVAvXtAB1lGUsi45cBw+aA3S8a5YS+3IxcAWPOkL54HLjx3m+XPQuKuU274twVMKA+c0Jj+2KcsE1E9yGg1Qk9AifAi3seBZpcewm4DyhX3zKw78ZKRg6JLP2sBGKrPkMrwqa/VpIpT2zchiefqr/IXiADpWEhkIdngRAYd57FQeAgxQO9C8lk6c9ECWVJqLiGkrAbKgXJql9vptacqMhYfOXEhFuGivukZ1xjVP1KR8mnIpMsPMnnc6C9dOsOOaGZCJVi7Xj68lKNaws9hi/JJZQTi1BZnroW4FvMlZCSzb0NGz+yHafx6weSXz+SxRxKY/8LCh1KY5jylqTahSiyF5RKPQ7E/hc0jJj/gr/PB0iCq/ZXNW7OAAAAAElFTkSuQmCC"
I_wifi_off="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAACXBIWXMAABYlAAAWJQFJUiTwAAACOklEQVR4Xs3XzctNURQG8J/v5KNEIVEyE8nEH0DkYyJKyUBhQBITMjOhDKkX+ZxI8TeYyUAZGCCESEpKyoSJr9ZpH527nXPvufd63+up2+10zl7rWWs9a+29+U+wHDewY1R8ruIXfmD/KEjsTM5HSmJfhcRPHJqITEytOAkNBKIck3EhPV+qIbIZ2zEL0/AZn/AMT/CUIoiBkGfiaGYlSH9NmomS1f0+4iYF0Qimb4QQI4LS+Fhm4W4PAlVSb3EwZaoVNuFx5qBOE/MwHVOwAGuwG+fxvIbgK2zsxiBSezaL/HvluV9hrsVFfKuQCRuXMSMnsgj3sg+jhisYujuW4HoW2AMKn38QSi/rFmremjH8Fy26Hu8rfjq6a2+aAfexrKFOB7IorrRSVCcWp0zHxA2fHYiersNMHKdQc95y3YbVSoq1dWjy9RdCtaHepn5vEuaJtOYNNjSQ6ImIOlJVOv+SWmwdRTnKd3UkohXLdfH+ZE9vGY5lBq5hfh/CnE2hkeowC5utcS4RiJG7q8uqXt0Ru2w5tsNma8zBEaxuWDEJq7CNwnC3coSNsBU2h0YYOYUPAwhzaEQkr1tuQq3HdqSyLd5hafo4puUtPErPMff3pE2pRHTBYWrPEwPhYYr+dkM95+JOi110YMT0CuF1y1opzpiQ3YQ5IejVokMjzg+ncYaOs2UV40oiTk7luN0yxLAaGAvxAi/zw0UNxjUTbREH3VKY8R9jesJRJRH3j5EgLsBxCYoL8ejxG/Fr561OWD92AAAAAElFTkSuQmCC"
}
# <<< generated icons
ic() { eval "echo templateImage=\$I_$1"; }

room_icon() { # pick an icon from the room name
  case "$(echo "$1" | tr '[:upper:]' '[:lower:]')" in
    *bed*|*nursery*|*guest*)            echo bed ;;
    *kitchen*|*dining*)                 echo cooking_pot ;;
    *living*|*lounge*|*family*)         echo sofa ;;
    *office*|*studio*|*desk*|*study*)   echo lamp ;;
    *)                                  echo house ;;
  esac
}

# Per-light icon overrides — exact light name (as in the LIFX app) = icon.
# Icons: lightbulb lamp lamp_desk lamp_floor lamp_ceiling strip flame grid
ICON_OVERRIDES="
LUNA Kitchen=lamp
"

light_icon() { # $1 = product type from LIFX, $2 = light name
  local o; o=$(printf '%s\n' "$ICON_OVERRIDES" | awk -F= -v n="$2" '$1==n {print $2; exit}')
  if [ -n "$o" ]; then echo "$o"; return; fi
  if [ "$1" != "bulb" ]; then echo "$1"; return; fi
  case "$(echo "$2" | tr '[:upper:]' '[:lower:]')" in
    *floor*)                                    echo lamp_floor ;;
    *desk*|*task*|*reading*)                    echo lamp_desk ;;
    *ceiling*|*pendant*|*overhead*|*chandelier*) echo lamp_ceiling ;;
    *lamp*)                                     echo lamp ;;
    *strip*)                                    echo strip ;;
    *)                                          echo lightbulb ;;
  esac
}

# ---------- Menu ----------
act() { echo "bash=\"$0\" param1=$1 param2=$2 ${3:+param3=$3} ${4:+param4=$4} terminal=false refresh=true"; }

WHITES="Candlelight|2000
Warm white|2700
Soft white|3500
Daylight|5000
Cool white|6500"
COLORS="Red Orange Yellow Green Cyan Blue Purple Pink"

controls() { # $1 = selector, $2 = nesting prefix, $3 = caps "color|temp|minK|maxK"
  local s="$1" p="$2" hc ht lo hi items="" def=""
  IFS='|' read -r hc ht lo hi <<< "$3"
  echo "${p}Toggle | $(act toggle "$s") $(ic power)"
  echo "${p}Brightness | $(act set "$s" brightness 1) $(ic sun)"
  for b in 10 25 50 75 100; do
    echo "${p}--${b}% | $(act set "$s" brightness "$(awk "BEGIN{print $b/100}")")"
  done
  # Only offer what this light (or every light in the room) supports
  if [ "$ht" = "true" ] || [ "$hc" = "true" ]; then
    while IFS='|' read -r label k; do
      if [ "$k" -ge "$lo" ] && [ "$k" -le "$hi" ]; then
        items="$items${p}--${label} | $(act set "$s" color "kelvin:$k")"$'\n'
        { [ -z "$def" ] || [ "$k" = 2700 ]; } && def="kelvin:$k"
      fi
    done <<< "$WHITES"
  fi
  if [ "$hc" = "true" ]; then
    for c in $COLORS; do
      items="$items${p}--${c} | $(act set "$s" color "$(echo "$c" | tr '[:upper:]' '[:lower:]')")"$'\n'
    done
  fi
  if [ -n "$items" ]; then
    if [ "$hc" = "true" ]; then label="Color"; else label="White"; fi
    echo "${p}${label} | $(act set "$s" color "$def") $(ic palette)"
    printf '%s' "$items"
  fi
}

LIGHTS=$(api "$API/lights/all")
if echo "$LIGHTS" | jq -e 'type=="object" and has("error")' >/dev/null 2>&1; then
  setup_menu "LIFX didn't accept your token: $(echo "$LIGHTS" | jq -r '.error' | tr '|' ' ')"
fi
if ! echo "$LIGHTS" | jq -e 'type=="array"' >/dev/null 2>&1; then
  echo "| sfimage=wifi.exclamationmark"; echo "---"
  echo "Couldn't reach LIFX Cloud"
  echo "Retry | refresh=true"
  exit 0
fi

ON=$(echo "$LIGHTS" | jq '[.[] | select(.power=="on" and .connected)] | length')
if [ "$ON" -gt 0 ]; then echo "| $(ic lightbulb)"; else echo "| $(ic lightbulb_off)"; fi
echo "---"
echo "All on | $(act set all power on) $(ic lightbulb)"
echo "All off | $(act set all power off) $(ic lightbulb_off)"
echo "---"

echo "$LIGHTS" | jq -r '
  def cap: (.product.capabilities // null) as $c |
    if $c == null then {c: true, t: true, lo: 1500, hi: 9000}
    else {c: ($c.has_color // false), t: ($c.has_variable_color_temp // false),
          lo: ($c.min_kelvin // 2500), hi: ($c.max_kelvin // 9000)} end;
  def fmt: "\(.c)|\(.t)|\(.lo)|\(.hi)";
  def clean: (. // "") | tostring | gsub("[|\t\n\r]"; " ") | sub("^-+"; "") | if . == "" then "Unnamed" else . end;
  map(.label |= clean | .group.name = ((.group.name // "Ungrouped") | clean) | .brightness = (.brightness // 0)) |
  group_by(.group.name)[] |
  ([.[] | cap] as $cs | {c: ($cs | all(.c)), t: ($cs | all(.t)),
     lo: ($cs | map(.lo) | max), hi: ($cs | map(.hi) | min)} | fmt) as $gcaps |
  "G\t\(if .[0].group.id then "group_id:\(.[0].group.id)" else (map("id:\(.id)") | join(",")) end)\t\(.[0].group.name)\t\([.[] | select(.power=="on")] | length)\t\(length)\t-\t-\t\($gcaps)",
  (sort_by(.label)[] |
    (.product.capabilities // {}) as $c | (.product.name // "") as $p |
    (if ($p | test("luna"; "i")) then "lamp"
     elif $c.has_matrix then "grid"
     elif $c.has_multizone then "strip"
     elif ($p | test("candle"; "i")) then "flame"
     elif ($p | test("strip|beam|neon|string"; "i")) then "strip"
     elif ($p | test("downlight"; "i")) then "lamp_ceiling"
     else "bulb" end) as $t |
    "L\t\(.id)\t\(.label)\t\(.power)\t\(.connected)\t\(.brightness)\t\($t)\t\(cap | fmt)")
' | while IFS=$'\t' read -r kind id name power connected bri ptype caps; do
  if [ "$kind" = "G" ]; then
    # here "id" = room selector, "power" = lights on, "connected" = total lights
    echo "$name  ($power/$connected on) | $(act toggle "$id") $(ic "$(room_icon "$name")")"
    controls "$id" "--" "$caps"
    echo "-----"
  else
    pct=$(awk "BEGIN{printf \"%d\", $bri*100}")
    if [ "$connected" != "true" ]; then
      echo "--$name — offline | color=gray $(ic wifi_off)"
    elif [ "$power" = "on" ]; then
      echo "--$name — $pct% | $(act toggle "id:$id") $(ic "$(light_icon "$ptype" "$name")")"
      controls "id:$id" "----" "$caps"
    else
      echo "--$name — off | $(act toggle "id:$id") $(ic "$(light_icon "$ptype" "$name")")"
      controls "id:$id" "----" "$caps"
    fi
  fi
done

echo "---"
echo "Refresh | refresh=true $(ic refresh_cw)"
echo "Change LIFX token… | bash=\"$0\" param1=settoken terminal=false refresh=true alternate=true $(ic key)"
echo "---"
echo "Made by | href=https://helenvholmes.com"
echo "--Helen V. Holmes | href=https://helenvholmes.com"
echo "--GitHub | href=https://github.com/helenvholmes"
