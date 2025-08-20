#!/bin/bash
# <xbar.title>NetStatus: Connectivity for Chinese User</xbar.title>
# <xbar.version>v2.1.7-beta</xbar.version>
# <xbar.author>ll</xbar.author>
# <xbar.author.github>aallGH</xbar.author.github>
# <xbar.desc>Monitors internet connectivity and latency to key websites, displaying real-time status in the menu bar.</xbar.desc>
# <xbar.dependencies>curl, bc</xbar.dependencies>
# <xbar.var>string(VAR_LOCAL_URL="https://www.baidu.com"): URL for local connectivity test</xbar.var>
# <xbar.var>string(VAR_OVERSEAS_URL="https://www.google.com"): URL for overseas connectivity test</xbar.var>


# ---------- Functions ----------
color_for_latency() {
  latency=$1
  if [ "$latency" -lt 200 ]; then
    echo "#058808"   # green
  elif [ "$latency" -lt 600 ]; then
    echo "#d1a500"   # yellow
  else
    echo "#850101"   # red
  fi
}

# ---------- Local (CN) ----------
local_latency=$(curl --silent --max-time 2 -o /dev/null -w "%{time_total}" "$VAR_LOCAL_URL")
local_dns_result=$?

# ---------- Overseas ----------
overseas_latency=$(curl --silent --max-time 2 -o /dev/null -w "%{time_total}" "$VAR_OVERSEAS_URL")
overseas_dns_result=$?

# ---------- Round and convert to ms ----------
local_latency_ms=$(printf "%.0f" "$(echo "$local_latency * 1000" | bc)")
overseas_latency_ms=$(printf "%.0f" "$(echo "$overseas_latency * 1000" | bc)")

# ---------- Status logic ----------
if [ $local_dns_result -eq 0 ] && [ $overseas_dns_result -eq 0 ]; then
  icon="🤩|dropdown=false"
  message="Internet OK|color=#058808"
  cn_icon="⚡️ ${local_latency_ms} ms|color=$(color_for_latency $local_latency_ms)"
  inter_icon="⚡️ ${overseas_latency_ms} ms|color=$(color_for_latency $overseas_latency_ms)"
elif [ $local_dns_result -eq 0 ] && [ $overseas_dns_result -ne 0 ]; then
  icon="🤪|dropdown=false"
  message="Online 🚫 NO Oversea|color=#850101"
  cn_icon="⚡️ ${local_latency_ms} ms|color=$(color_for_latency $local_latency_ms)"
  inter_icon="🚫"
elif [ $local_dns_result -ne 0 ] && [ $overseas_dns_result -eq 0 ]; then
  icon="🤪|dropdown=false"
  message="Online 🚫 NO CN|color=#850101"
  cn_icon="🚫"
  inter_icon="⚡️ ${overseas_latency_ms} ms|color=$(color_for_latency $overseas_latency_ms)"
else
  icon="👾|dropdown=false"
  message="No Internet!!!|color=#850101"
  cn_icon="🚫"
  inter_icon="🚫"
fi

# ---------- Output ----------
echo "$icon"
echo "---"
echo "Internet CN: $cn_icon"
echo "Internet Oversea: $inter_icon"
echo "---"
echo "$message"
echo "---"
echo "Last checked: $(date '+%H:%M:%S')"
