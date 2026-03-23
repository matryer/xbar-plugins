#!/bin/zsh

# <xbar.title>Netlify Deployments</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>David Ollerhead</xbar.author>
# <xbar.author.github>oller</xbar.author.github>
# <xbar.desc>Monitor the status and history of your recent Netlify deployments with visual indicators and clickable links.</xbar.desc>
# <xbar.dependencies>jq,curl</xbar.dependencies>
# <xbar.var>string(NETLIFY_AUTH_TOKEN=""): Your Netlify auth token. Get it from: https://app.netlify.com/user/applications#personal-access-tokens</xbar.var>
# <xbar.var>string(NETLIFY_SITE_NAME=""): Your Netlify site name (e.g., 'vim-gym').</xbar.var>

# Ensure a standard PATH is available (xbar runs with minimal PATH)
# This ensures we can find jq and curl even if installed via Homebrew
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/homebrew/bin:$PATH"

NETLIFY_ICON_BASE64="PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHN2ZyBpZD0ibG9nb3NhbmR0eXBlc19jb20iIGRhdGEtbmFtZT0ibG9nb3NhbmR0eXBlcyBjb20iIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgdmlld0JveD0iMCAwIDE1MCAxNTAiIHdpZHRoPSIxOCIgaGVpZ2h0PSIxOCI+CiAgPGRlZnM+CiAgICA8c3R5bGU+CiAgICAgIC5jbHMtMSB7CiAgICAgICAgZmlsbDogIzAxNDg0NzsKICAgICAgfQoKICAgICAgLmNscy0yIHsKICAgICAgICBmaWxsOiBub25lOwogICAgICB9CgogICAgICAuY2xzLTMgewogICAgICAgIGZpbGw6ICMwNWJkYmE7CiAgICAgIH0KICAgIDwvc3R5bGU+CiAgPC9kZWZzPgogIDxwYXRoIGNsYXNzPSJjbHMtMiIgZD0iTTAsMEgxNTBWMTUwSDBWMFoiLz4KICA8cGF0aCBjbGFzcz0iY2xzLTMiIGQ9Ik00My45MSwxMTYuNjRoLTEuMzRsLTYuNjctNi42N3YtMS4zNGwxMC4xOS0xMC4xOWg3LjA2bC45NCwuOTR2Ny4wNmwtMTAuMTksMTAuMTlaIi8+CiAgPHBhdGggY2xhc3M9ImNscy0zIiBkPSJNMzUuOSw0MS4yMnYtMS4zNGw2LjY3LTYuNjdoMS4zNGwxMC4xOSwxMC4xOXY3LjA2bC0uOTQsLjk0aC03LjA2bC0xMC4xOS0xMC4xOVoiLz4KICA8cGF0aCBjbGFzcz0iY2xzLTEiIGQ9Ik05NC42LDk1LjE0aC05LjdsLS44MS0uODF2LTIyLjcxYzAtNC4wNC0xLjU5LTcuMTctNi40Ni03LjI4LTIuNTEtLjA3LTUuMzgsMC04LjQ0LC4xMmwtLjQ2LC40N3YyOS4zOWwtLjgxLC44MWgtOS43bC0uODEtLjgxVjU1LjUzbC44MS0uODFoMjEuODNjOC40OCwwLDE1LjM2LDYuODgsMTUuMzYsMTUuMzZ2MjQuMjVsLS44MSwuODFaIi8+CiAgPHBhdGggY2xhc3M9ImNscy0zIiBkPSJNNDUuMjksODAuNkg2LjQ5bC0uODEtLjgxdi0yLjcybC44MS0uODFINDUuMjlsLjgxLC44MXY5LjcybC0uODEsLjgxWiIvPgogIDxwYXRoIGNsYXNzPSJjbHMtMyIgZD0iTTE0Ni4zNCw4MC42aC0zOC44bC0uODEtLjgxdi05LjcybC44MS0uODFoMzguOGwuODEsLjgxdjkuNzJsLS44MSwuODFaIi8+CiAgPHBhdGggY2xhc3M9ImNscy0zIiBkPSJNNzAuODIsNDIuNlYxMy41bC44MS0uODFoOS43MmwuODEsLjgxdjI5LjFsLS44MSwuODFoLTkuNzJsLS44MS0uODFaIi8+CiAgPHBhdGggY2xhc3M9ImNscy0zIiBkPSJNNzAuODIsMTM2LjM2di0yOS4xbC44MS0uODFoOS43MmwuODEsLjgxdjI5LjFsLS44MSwuODFoLTkuNzJsLS44MS0uODFaIi8+Cjwvc3ZnPg=="

# Check for required dependencies
if ! command -v jq &>/dev/null; then
	echo "⚠️ Missing dependency"
	echo "---"
	echo "jq is required but not installed"
	echo "Install via Homebrew: brew install jq"
	exit 0
fi

if ! command -v curl &>/dev/null; then
	echo "⚠️ Missing dependency"
	echo "---"
	echo "curl is required but not installed"
	exit 0
fi

# Colors (Hex for xbar)
COLOR_GREY="#8a8a8a"

# URLs
BASE_URL="https://app.netlify.com/projects/$NETLIFY_SITE_NAME"

get_relative_time() {
	local created_at="$1"
	local now=$(date +%s)
	local created_seconds=$(date -j -f "%Y-%m-%dT%H:%M:%SZ" "$created_at" "+%s" 2>/dev/null)

	if [ -z "$created_seconds" ]; then
		local clean_date=$(echo "$created_at" | sed 's/Z$//')
		created_seconds=$(date -j -f "%Y-%m-%dT%H:%M:%S" "$clean_date" "+%s" 2>/dev/null)
	fi

	local diff=$((now - created_seconds))

	if [ $diff -lt 60 ]; then
		echo "${diff}s ago"
	elif [ $diff -lt 3600 ]; then
		echo "$((diff / 60))m ago"
	elif [ $diff -lt 86400 ]; then
		echo "$((diff / 3600))h ago"
	else
		echo "$((diff / 86400))d ago"
	fi
}

if [ -z "$NETLIFY_AUTH_TOKEN" ]; then
	echo "⚠️ No token | templateImage=$NETLIFY_ICON_BASE64"
	echo "---"
	echo "Set NETLIFY_AUTH_TOKEN environment variable"
	echo "Get your token: https://app.netlify.com/user/applications#personal-access-tokens | href=https://app.netlify.com/user/applications#personal-access-tokens"
	exit 0
fi

if [ -z "$NETLIFY_SITE_NAME" ]; then
	echo "⚠️ No site name | templateImage=$NETLIFY_ICON_BASE64"
	echo "---"
	echo "Set NETLIFY_SITE_NAME environment variable"
	echo "Example: vim-gym or your-site-name"
	exit 0
fi

# Common Headers
CURL_AUTH=(-H "Authorization: Bearer $NETLIFY_AUTH_TOKEN")

get_site_id() {
	local response=$(curl -s -w "\n%{http_code}" "${CURL_AUTH[@]}" "https://api.netlify.com/api/v1/sites")
	local http_code=$(echo "$response" | tail -n1)
	local body=$(echo "$response" | sed '$d')

	# Check for HTTP errors
	if [ "$http_code" != "200" ]; then
		echo "ERROR:HTTP_$http_code"
		return 1
	fi

	# Check if we got empty response
	if [ -z "$body" ]; then
		echo "ERROR:EMPTY_RESPONSE"
		return 1
	fi

	# Try to find by name first
	# Clean control characters before passing to jq
	local site_id=$(printf '%s' "$body" | tr -d '\000-\037' | jq -r ".[] | select(.name == \"$NETLIFY_SITE_NAME\") | .id" 2>/dev/null | head -1)

	# If not found, try by slug
	if [ -z "$site_id" ]; then
		site_id=$(printf '%s' "$body" | tr -d '\000-\037' | jq -r ".[] | select(.slug == \"$NETLIFY_SITE_NAME\") | .id" 2>/dev/null | head -1)
	fi

	echo "$site_id"
}

SITE_ID=$(get_site_id)

# Check for API errors
if [[ "$SITE_ID" == ERROR:* ]]; then
	error_type="${SITE_ID#ERROR:}"
	echo "⚠️ API Error | templateImage=$NETLIFY_ICON_BASE64"
	echo "---"
	if [[ "$error_type" == HTTP_* ]]; then
		echo "HTTP Error: ${error_type#HTTP_}"
		echo "Check your NETLIFY_AUTH_TOKEN"
	elif [[ "$error_type" == "EMPTY_RESPONSE" ]]; then
		echo "Empty API response"
		echo "The Netlify API may be down"
	fi
	echo "---"
	echo "Check Netlify status | href=https://www.netlifystatus.com/"
	exit 0
fi

if [ -z "$SITE_ID" ]; then
	echo "⚠️ Site not found | templateImage=$NETLIFY_ICON_BASE64"
	echo "---"
	echo "Site name '$NETLIFY_SITE_NAME' not found"
	echo "---"
	echo "Check your Netlify sites | href=https://app.netlify.com/sites"
	echo "Verify the site name matches exactly"
	exit 0
fi

# Fetch latest deploys (sorted by created_at descending by default)
# Add timestamp to prevent caching
response=$(curl -s -w "\n%{http_code}" "${CURL_AUTH[@]}" "https://api.netlify.com/api/v1/sites/$SITE_ID/deploys?per_page=10&_=$(date +%s)")
http_code=$(echo "$response" | tail -n1)
body=$(echo "$response" | sed '$d')

# Check for HTTP errors
if [ "$http_code" != "200" ]; then
	echo "⚠️ API Error | templateImage=$NETLIFY_ICON_BASE64"
	echo "---"
	echo "Failed to fetch deployments (HTTP $http_code)"
	echo "---"
	echo "Check Netlify status | href=https://www.netlifystatus.com/"
	exit 0
fi

# Check if we got any deploys
# Clean control characters before passing to jq
deploy_count=$(printf '%s' "$body" | tr -d '\000-\037' | jq 'length' 2>/dev/null)

if [ -z "$deploy_count" ] || [ "$deploy_count" = "0" ]; then
	echo "📭 No deployments | templateImage=$NETLIFY_ICON_BASE64"
	echo "---"
	echo "No deployments found for $NETLIFY_SITE_NAME"
	echo "---"
	echo "View site settings | href=https://app.netlify.com/projects/$NETLIFY_SITE_NAME"
	exit 0
fi

latest_state=$(printf '%s' "$body" | tr -d '\000-\037' | jq -r '.[0].state')

# Emoji icons for status indicators (rich emojis for dropdown)
STATUS_ICON_SUCCESS_RICH="✅"
STATUS_ICON_FAILED_RICH="💥"
STATUS_ICON_PENDING_RICH="⏳"
STATUS_ICON_UNKNOWN_RICH="❓"

# Simple icons for menu bar
STATUS_ICON_SUCCESS_SIMPLE="✓"
STATUS_ICON_FAILED_SIMPLE="✗"
STATUS_ICON_PENDING_SIMPLE="⏳"
STATUS_ICON_UNKNOWN_SIMPLE="?"

# Helper to map state to icons
set_status_assets() {
	case "$1" in
	"ready")
		STATUS_ICON_RICH="$STATUS_ICON_SUCCESS_RICH"
		STATUS_ICON_SIMPLE="$STATUS_ICON_SUCCESS_SIMPLE"
		STATUS_NAME="Success"
		;;
	"failed" | "error")
		STATUS_ICON_RICH="$STATUS_ICON_FAILED_RICH"
		STATUS_ICON_SIMPLE="$STATUS_ICON_FAILED_SIMPLE"
		STATUS_NAME="Failed"
		;;
	"new" | "enqueued" | "building" | "processing" | "uploading" | "uploaded" | "preparing" | "prepared")
		STATUS_ICON_RICH="$STATUS_ICON_PENDING_RICH"
		STATUS_ICON_SIMPLE="$STATUS_ICON_PENDING_SIMPLE"
		STATUS_NAME="Pending"
		;;
	*)
		STATUS_ICON_RICH="$STATUS_ICON_UNKNOWN_RICH"
		STATUS_ICON_SIMPLE="$STATUS_ICON_UNKNOWN_SIMPLE"
		STATUS_NAME="Unknown"
		;;
	esac
}

# Top bar: Netlify Icon with status (simple icon)
set_status_assets "$latest_state"
echo "$STATUS_ICON_SIMPLE | templateImage=$NETLIFY_ICON_BASE64"
echo "---"
echo "$NETLIFY_SITE_NAME deployments | color=white"

printf '%s' "$body" | tr -d '\000-\037' | jq -r '.[:5] | to_entries | .[] | "\(.value.state)|\(.value.id)|\(.value.created_at)"' | while IFS='|' read -r status_raw deploy_id created_at; do
	deploy_id=$(echo "$deploy_id" | tr -d ' ')
	relative_time=$(get_relative_time "$created_at")

	set_status_assets "$status_raw"

	# Format with fixed-width spacing for alignment (rich emoji for dropdown)
	# Using printf to pad the status name to create visual columns
	printf "%s  %-10s  %s | color=%s href=%s\n" "$STATUS_ICON_RICH" "$STATUS_NAME" "$relative_time" "$COLOR_GREY" "$BASE_URL/deploys/$deploy_id"
done

echo "---"
echo "View all deployments| href=$BASE_URL/deploys"
echo "---"
echo "Last refreshed: $(date '+%H:%M:%S') | color=$COLOR_GREY"
