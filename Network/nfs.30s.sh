#!/usr/bin/env bash
# SwiftBar plugin: NFS automount status + recovery
#
# <bitbar.title>NFS Mounts</bitbar.title>
# <bitbar.version>v26.05.1</bitbar.version>
# <bitbar.author>Gareth Evans</bitbar.author>
# <bitbar.author.github>ggfevans</bitbar.author.github>
# <bitbar.desc>Status of autofs NFS shares (active / idle / unreachable) with a sudo force-remount recovery action. Reads mounts from /etc/auto_direct.</bitbar.desc>
# <bitbar.dependencies>bash,mount,nc,df,osascript,perl</bitbar.dependencies>
# <swiftbar.hideAbout>true</swiftbar.hideAbout>
#
# <xbar.title>NFS Mounts</xbar.title>
# <xbar.version>v26.05.1</xbar.version>
# <xbar.author>Gareth Evans</xbar.author>
# <xbar.author.github>ggfevans</xbar.author.github>
# <xbar.desc>Status of autofs NFS shares (active / idle / unreachable) with a sudo force-remount recovery action. Reads mounts from /etc/auto_direct.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/ggfevans/nfs-swiftbar/main/assets/nfs-swiftbar-screenshot.png</xbar.image>
# <xbar.abouturl>https://github.com/ggfevans/nfs-swiftbar</xbar.abouturl>
# <xbar.dependencies>bash,mount,nc,df,osascript,perl</xbar.dependencies>
#
# Design note: the 30s poll deliberately NEVER accesses the mount paths
# (no ls/df/stat on them) — doing so would re-trigger the automounter every
# cycle and defeat the autofs idle timeout. State comes from the `mount` table
# and reachability from a TCP probe of the nfsd port (2049) — so "reachable"
# means the NFS service is actually answering, not just that the host pings.
# df (free space) is click-only, never in the poll. Touching a path (Reveal in
# Finder) is a deliberate user action.
#
# Run `nfs.30s.sh --selftest` to render all three states from fixture data
# without touching real mounts or the network.

set -u

MAP="/etc/auto_direct"

# Colours
GREEN="#16a34a"   # active (mounted now)
GREY="#9ca3af"    # idle but reachable (healthy — just past idle timeout)
RED="#dc2626"     # host unreachable

# Resolve our own absolute path so menu actions can call back into us.
SELF="$0"
case "$SELF" in
    /*) ;;
    *) SELF="$(cd "$(dirname "$SELF")" 2>/dev/null && pwd)/$(basename "$SELF")" ;;
esac

# ---------------------------------------------------------------------------
# Helpers shared by the action dispatch and the renderer.
# ---------------------------------------------------------------------------

# as_escape <string> — escape a value for safe embedding inside an AppleScript
# "double-quoted" string literal. Backslashes first, then double quotes.
as_escape() {
    local s="$1"
    s=${s//\\/\\\\}
    s=${s//\"/\\\"}
    printf '%s' "$s"
}

# notify <title> <message> — macOS notification, both fields AppleScript-escaped.
notify() {
    osascript -e "display notification \"$(as_escape "$2")\" with title \"$(as_escape "$1")\"" >/dev/null 2>&1
}

# is_mounted <mountpoint> — true if currently in the mount table as nfs.
is_mounted() {
    /sbin/mount | grep -q " on $1 (nfs"
}

# read_mountpoints — populate the global `mps` array from the map (one entry
# per valid line). Leaves `mps` empty if the map is unreadable or has no shares.
read_mountpoints() {
    mps=()
    [ -r "$MAP" ] || return
    local mp target _
    while read -r mp _ target _ || [ -n "$mp" ]; do
        case "$mp" in ''|\#*) continue ;; esac
        [ -n "$target" ] || continue
        mps+=("$mp")
    done < "$MAP"
}

# sudo_umount_all <mountpoint>... — chain `umount -f` for every given path into
# ONE privileged osascript call, so the admin prompt appears once. `umount -f`
# (forced) is used so a dead/unreachable server can't hang the unmount. Each
# path is printf %q-quoted for the inner /bin/sh, then AppleScript-escaped.
sudo_umount_all() {
    local cmd="" mp
    for mp in "$@"; do
        cmd="$cmd/sbin/umount -f $(printf '%q' "$mp"); "
    done
    osascript -e "do shell script \"$(as_escape "$cmd")\" with administrator privileges" >/dev/null 2>&1
}

# retrigger <mountpoint> — access the path as the normal user to let autofs
# re-mount, bounded by a 5s alarm so a dead server can't hang us (macOS has no
# `timeout(1)`; perl is stock). Best-effort: a hard mount wedged in
# uninterruptible I/O may still block.
retrigger() {
    perl -e 'alarm 5; exec @ARGV' /bin/ls "$1" >/dev/null 2>&1
}

# ---------------------------------------------------------------------------
# Action dispatch: `nfs.30s.sh remount <mountpoint>`
# Prompts once for admin rights, force-unmounts the (possibly stale) handle,
# then re-accesses the path to let autofs re-trigger a fresh mount.
# ---------------------------------------------------------------------------
if [ "${1:-}" = "remount" ]; then
    mp="${2:-}"
    [ -n "$mp" ] || exit 0
    name="${mp#/Volumes/}"
    sudo_umount_all "$mp"
    retrigger "$mp"
    if is_mounted "$mp"; then
        notify "NFS · $name" "Remounted OK"
    else
        notify "NFS · $name" "Remount failed — host may be unreachable"
    fi
    exit 0
fi

# `nfs.30s.sh unmount <mountpoint>` — force-unmount a single share WITHOUT
# re-triggering it (the share drops to idle immediately). autofs will still
# re-mount on next access; this just detaches it now.
if [ "${1:-}" = "unmount" ]; then
    mp="${2:-}"
    [ -n "$mp" ] || exit 0
    name="${mp#/Volumes/}"
    sudo_umount_all "$mp"
    if is_mounted "$mp"; then
        notify "NFS · $name" "Unmount failed (busy?)"
    else
        notify "NFS · $name" "Unmounted"
    fi
    exit 0
fi

# `nfs.30s.sh freespace <mountpoint>` — deliberate, user-initiated df.
# Reports capacity in a notification. (df accesses the share, so this is kept
# out of the poll to preserve the idle timeout — it only runs on a click.)
if [ "${1:-}" = "freespace" ]; then
    mp="${2:-}"
    [ -n "$mp" ] || exit 0
    name="${mp#/Volumes/}"
    summary="$(df -h "$mp" 2>/dev/null | awk 'NR==2 {printf "Size %s · Used %s · Free %s (%s used)", $2, $3, $4, $5}')"
    [ -n "$summary" ] || summary="unavailable (mount not responding)"
    notify "NFS · $name" "$summary"
    exit 0
fi

# `nfs.30s.sh remount-all` — force-remount every share in the map. The chained
# `umount -f` (one prompt) is followed by a bounded re-trigger of each share.
if [ "${1:-}" = "remount-all" ]; then
    read_mountpoints
    [ "${#mps[@]}" -gt 0 ] || exit 0
    sudo_umount_all "${mps[@]}"
    ok=0
    for mp in "${mps[@]}"; do
        retrigger "$mp"
        is_mounted "$mp" && ok=$((ok + 1))
    done
    notify "NFS" "Force-remounted $ok/${#mps[@]} share(s)"
    exit 0
fi

# `nfs.30s.sh unmount-all` — force-unmount every share in the map (one prompt),
# WITHOUT re-triggering. Everything drops to idle until next access.
if [ "${1:-}" = "unmount-all" ]; then
    read_mountpoints
    [ "${#mps[@]}" -gt 0 ] || exit 0
    sudo_umount_all "${mps[@]}"
    done_count=0
    for mp in "${mps[@]}"; do
        is_mounted "$mp" || done_count=$((done_count + 1))
    done
    notify "NFS" "Unmounted $done_count/${#mps[@]} share(s)"
    exit 0
fi

# ---------------------------------------------------------------------------
# Rendering helpers — shared by the live poll and --selftest so formatting,
# icons and colours can never drift between them.
# ---------------------------------------------------------------------------
DROPDOWN=""
append() { DROPDOWN="${DROPDOWN}$1
"; }

# emit_share <mountpoint> <active|idle|unreachable>
emit_share() {
    local mp="$1" state="$2" name="${1#/Volumes/}"
    case "$state" in
        active)      append "$name · active | sfimage=circle.fill color=$GREEN bash=\"open\" param1=\"$mp\" terminal=false refresh=false" ;;
        idle)        append "$name · idle | sfimage=circle color=$GREY bash=\"open\" param1=\"$mp\" terminal=false refresh=true" ;;
        unreachable) append "$name · unreachable | sfimage=circle.fill color=$RED bash=\"open\" param1=\"$mp\" terminal=false refresh=true" ;;
    esac
    append "-- Reveal in Finder | bash=\"open\" param1=\"$mp\" terminal=false refresh=true"
    # Free space and Unmount only make sense for a mounted share, so offer them
    # only when active (free space would also automount an idle one via df).
    if [ "$state" = "active" ]; then
        append "-- Show free space | bash=\"$SELF\" param1=\"freespace\" param2=\"$mp\" terminal=false refresh=false"
        append "-- Unmount (sudo) | bash=\"$SELF\" param1=\"unmount\" param2=\"$mp\" terminal=false refresh=true"
    fi
    append "-- Force remount (sudo) | bash=\"$SELF\" param1=\"remount\" param2=\"$mp\" terminal=false refresh=true"
}

# menubar_icon <unreachable_count> <active_count> — red if any host down,
# green if any mount active, else grey.
menubar_icon() {
    if [ "$1" -gt 0 ]; then
        echo "| sfimage=externaldrive.badge.xmark color=$RED"
    elif [ "$2" -gt 0 ]; then
        echo "| sfimage=externaldrive.fill color=$GREEN"
    else
        echo "| sfimage=externaldrive color=$GREY"
    fi
}

# ---------------------------------------------------------------------------
# `nfs.30s.sh --selftest` — render all three states from fixture data, with no
# access to real mounts or the network. For safely eyeballing icons/colours.
# ---------------------------------------------------------------------------
if [ "${1:-}" = "--selftest" ]; then
    menubar_icon 1 1   # fixture has 1 unreachable + 1 active → exercises the red icon
    echo "---"
    echo "NFS Mounts · SELF-TEST (fixture, not live) | disabled=true"
    echo "---"
    emit_share "/Volumes/demo-active"      active
    emit_share "/Volumes/demo-idle"        idle
    emit_share "/Volumes/demo-unreachable" unreachable
    printf '%s' "$DROPDOWN"
    echo "---"
    echo "Force remount all (sudo) | bash=\"$SELF\" param1=\"remount-all\" terminal=false refresh=true"
    echo "Unmount all (sudo) | bash=\"$SELF\" param1=\"unmount-all\" terminal=false refresh=true"
    echo "Refresh | bash=\"true\" terminal=false refresh=true"
    exit 0
fi

# ---------------------------------------------------------------------------
# Live status helpers
# ---------------------------------------------------------------------------

# Memoised NFS reachability — TCP-connect to the nfsd port (2049) so a share is
# "reachable" only when the NFS service is actually listening, not merely when
# the host answers ping. Each host is probed once per run (-G2 = 2s connect cap).
REACHABLE_HOSTS=""
UNREACHABLE_HOSTS=""
nfs_reachable() {
    local h="$1"
    case " $REACHABLE_HOSTS " in *" $h "*) return 0 ;; esac
    case " $UNREACHABLE_HOSTS " in *" $h "*) return 1 ;; esac
    if nc -z -G2 "$h" 2049 >/dev/null 2>&1; then
        REACHABLE_HOSTS="$REACHABLE_HOSTS $h"
        return 0
    fi
    UNREACHABLE_HOSTS="$UNREACHABLE_HOSTS $h"
    return 1
}

# ---------------------------------------------------------------------------
# Bail early if the map is missing — render red, no actions.
# ---------------------------------------------------------------------------
if [ ! -r "$MAP" ]; then
    echo "| sfimage=externaldrive.badge.questionmark color=$RED"
    echo "---"
    echo "NFS · $MAP missing | disabled=true"
    exit 0
fi

# ---------------------------------------------------------------------------
# Walk the map, build the dropdown, tally state for the menu-bar icon.
# Read from file (not a pipe) so counters survive in this shell.
# ---------------------------------------------------------------------------
TOTAL=0
ACTIVE=0
UNREACHABLE=0

# `|| [ -n "$mp" ]` so a final line lacking a trailing newline isn't dropped.
while read -r mp _ target _ || [ -n "$mp" ]; do
    case "$mp" in ''|\#*) continue ;; esac      # skip blanks/comments
    [ -n "$target" ] || continue                # skip malformed lines

    host="${target%%:*}"
    TOTAL=$((TOTAL + 1))

    if is_mounted "$mp"; then
        ACTIVE=$((ACTIVE + 1))
        emit_share "$mp" active
    elif nfs_reachable "$host"; then
        emit_share "$mp" idle
    else
        UNREACHABLE=$((UNREACHABLE + 1))
        emit_share "$mp" unreachable
    fi
done < "$MAP"

# Menu-bar line first, as SwiftBar requires.
menubar_icon "$UNREACHABLE" "$ACTIVE"
echo "---"
echo "NFS Mounts · ${ACTIVE}/${TOTAL} active | disabled=true"
echo "---"
printf '%s' "$DROPDOWN"
echo "---"
if [ "$TOTAL" -gt 0 ]; then
    echo "Force remount all (sudo) | bash=\"$SELF\" param1=\"remount-all\" terminal=false refresh=true"
    echo "Unmount all (sudo) | bash=\"$SELF\" param1=\"unmount-all\" terminal=false refresh=true"
fi
echo "Refresh | bash=\"true\" terminal=false refresh=true"
