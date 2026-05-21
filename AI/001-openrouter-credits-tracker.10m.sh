#!/bin/zsh

# Metadata allows the plugin to show up in the xbar app and website.
#
#  <xbar.title>OpenRouter credits tracker</xbar.title>
#  <xbar.version>v0.1</xbar.version>
#  <xbar.author>Guillaume Jacquet</xbar.author>
#  <xbar.author.github>guillaume-jacquet</xbar.author.github>
#  <xbar.desc>Display Openrouter credits informations</xbar.desc>
#  <xbar.var>string(VAR_MENU_BAR_TEXT='🤖'): The text that will appear in the Menu Bar</xbar.var>
#  <xbar.var>string(VAR_OPEN_ROUTER_API_KEY=''): Your OpenRouter API Key</xbar.var>
#  <xbar.var>number(VAR_LOW_ALERT_LIMIT=2.0): Low limit for your credits</xbar.var>
#  <xbar.var>number(VAR_MEDIUM_ALERT_LIMIT=5.0): Medium limit for your credits</xbar.var>
#  <xbar.var>string(VAR_LOW_ALERT_SYMBOL='🔴'): Low symbol for your credits</xbar.var>
#  <xbar.var>string(VAR_MEDIUM_ALERT_SYMBOL='🔵'): Medium symbol for your credits</xbar.var>
#  <xbar.var>string(VAR_HIGH_ALERT_SYMBOL='🟢'): High symbol for your credits</xbar.var>

if [[ -z "$VAR_OPEN_ROUTER_API_KEY" ]]; then
    echo "$VAR_MENU_BAR_TEXT ⚠️"
    echo "---"
    echo "API key not set | color=red"
    exit 0
fi

RESPONSE=$(curl --silent https://openrouter.ai/api/v1/credits \
    -H "Authorization: Bearer $VAR_OPEN_ROUTER_API_KEY" \
    -H "Content-Type: application/json")
curl_exit=$?

if [[ $curl_exit -ne 0 ]] || ! jq -e '.data' <<< "$RESPONSE" > /dev/null 2>&1 || jq -e '.error' <<< "$RESPONSE" > /dev/null 2>&1; then
    error_msg=$(jq -r '.error.message // "Failed to fetch credits"' <<< "$RESPONSE" 2>/dev/null)
    echo "$VAR_MENU_BAR_TEXT ⚠️"
    echo "---"
    echo "$error_msg | color=red"
    exit 0
fi

read -r total_credits_raw usage_raw <<< "$(jq -r '[.data.total_credits, .data.total_usage] | @tsv' <<< "$RESPONSE")"
remaining_credits_raw=$(echo "$total_credits_raw - $usage_raw" | bc -l)

total_credits=$(printf "%.2f" "$total_credits_raw")
total_usage=$(printf "%.2f" "$usage_raw")
remaining_credits=$(printf "%.2f" "$remaining_credits_raw")

if (( $(echo "$remaining_credits < $VAR_LOW_ALERT_LIMIT" | bc -l) )); then
    alert_symbol="$VAR_LOW_ALERT_SYMBOL"
elif (( $(echo "$remaining_credits < $VAR_MEDIUM_ALERT_LIMIT" | bc -l) )); then
    alert_symbol="$VAR_MEDIUM_ALERT_SYMBOL"
else
    alert_symbol="$VAR_HIGH_ALERT_SYMBOL"
fi

echo "$VAR_MENU_BAR_TEXT $alert_symbol"
echo "---"
echo "Credits: +$total_credits€ | size=16"
echo "Usage: -$total_usage€ | size=16"
echo "Remaining: $remaining_credits€ $alert_symbol | size=16"
echo "OpenRouter 🌍"
echo "-- OpenRouter - Activity | href=https://openrouter.ai/activity"
echo "-- OpenRouter - Credits | href=https://openrouter.ai/credits"
echo "---"
echo "Refresh | terminal=false refresh=true"
