#!/usr/bin/env bash

# xbar Plugin Metadata (compatible with xbar and SwiftBar)
# <xbar.title>OpenAI API Usage & Costs</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>domdewom</xbar.author>
# <xbar.author.github>domdewom</xbar.author.github>
# <xbar.desc>Monitor OpenAI API usage and costs in your menu bar</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/domdewom/openai_usage/main/oh_usage.png</xbar.image>
# <xbar.dependencies>curl,jq</xbar.dependencies>
# <xbar.abouturl>https://platform.openai.com/usage</xbar.abouturl>

# SwiftBar Plugin Metadata
# <swiftbar.title>OpenAI API Usage & Costs</swiftbar.title>
# <swiftbar.version>1.0</swiftbar.version>
# <swiftbar.author>domdewom</swiftbar.author>
# <swiftbar.desc>Monitor OpenAI API usage and costs in your menu bar</swiftbar.desc>
# <swiftbar.dependencies>curl,jq</swiftbar.dependencies>
# <swiftbar.abouturl>https://platform.openai.com/usage</swiftbar.abouturl>

# Load API key (tries multiple methods in order of security)
# Method 1: macOS Keychain (most secure)
ADMIN_KEY=$(security find-generic-password -a "$USER" -s "openai_admin_key" -w 2>/dev/null)

# Method 2: Environment variable (works in terminal)
if [ -z "$ADMIN_KEY" ] && [ -n "$OPENAI_ADMIN_KEY" ]; then
    ADMIN_KEY="$OPENAI_ADMIN_KEY"
fi

# Method 3: Config file (works for GUI apps like SwiftBar)
if [ -z "$ADMIN_KEY" ] && [ -f "$HOME/.openai_admin_key" ]; then
    ADMIN_KEY=$(cat "$HOME/.openai_admin_key" | tr -d '\n' | tr -d ' ')
fi

# Method 4: Alternative config location
if [ -z "$ADMIN_KEY" ] && [ -f "$HOME/.config/openai/admin_key" ]; then
    ADMIN_KEY=$(cat "$HOME/.config/openai/admin_key" | tr -d '\n' | tr -d ' ')
fi

# If still not found, show helpful error
if [ -z "$ADMIN_KEY" ]; then
    echo "⚠️ OpenAI | color=red"
    echo "---"
    echo "Error: API key not found | color=red"
    echo "---"
    echo "Option 1: macOS Keychain (Most Secure) | font=Menlo-Bold"
    echo "security add-generic-password -a \"\$USER\" -s \"openai_admin_key\" -w \"your-key\" | font=Menlo size=9 color=#888888"
    echo "---"
    echo "Option 2: Config File | font=Menlo-Bold"
    echo "echo 'your-key' > ~/.openai_admin_key | font=Menlo size=9 color=#888888"
    echo "chmod 600 ~/.openai_admin_key | font=Menlo size=9 color=#888888"
    echo "---"
    echo "Option 3: Environment Variable | font=Menlo-Bold"
    echo "export OPENAI_ADMIN_KEY='your-key' | font=Menlo size=9 color=#888888"
    echo "---"
    echo "Then refresh this plugin"
    exit 1
fi

# Calculate billing period: start of current month to now
START_DATE=$(date +"%Y-%m-01")
END_DATE=$(date +"%Y-%m-%d")

# Convert to Unix timestamps (macOS date format)
START_TIME=$(date -j -f "%Y-%m-%d" "$START_DATE" "+%s")
END_TIME=$(date +%s)

# Function to fetch all paginated data from Usage API
fetch_usage_data() {
    local url="$1"
    local all_data="[]"
    local page_cursor=""
    
    while true; do
        local params="start_time=$START_TIME&end_time=$END_TIME&bucket_width=1d"
        if [ -n "$page_cursor" ]; then
            params="$params&page=$page_cursor"
        fi
        
        local response=$(curl -s \
            "$url?$params" \
            -H "Authorization: Bearer $ADMIN_KEY")
        
        # Check if response is valid JSON
        if ! echo "$response" | jq . >/dev/null 2>&1; then
            echo "[]"
            return 1
        fi
        
        # Extract data array and merge with existing data
        local page_data=$(echo "$response" | jq '.data // []')
        all_data=$(echo "$all_data $page_data" | jq -s 'add')
        
        # Check for next page
        page_cursor=$(echo "$response" | jq -r '.next_page // empty')
        if [ -z "$page_cursor" ] || [ "$page_cursor" = "null" ]; then
            break
        fi
    done
    
    echo "$all_data"
}

# Function to fetch all paginated data from Costs API
fetch_costs_data() {
    local url="https://api.openai.com/v1/organization/costs"
    local all_data="[]"
    local page_cursor=""
    
    while true; do
        local params="start_time=$START_TIME&end_time=$END_TIME"
        if [ -n "$page_cursor" ]; then
            params="$params&page=$page_cursor"
        fi
        
        local response=$(curl -s \
            "$url?$params" \
            -H "Authorization: Bearer $ADMIN_KEY")
        
        # Check if response is valid JSON
        if ! echo "$response" | jq . >/dev/null 2>&1; then
            echo "[]"
            return 1
        fi
        
        # Extract data array and merge with existing data
        local page_data=$(echo "$response" | jq '.data // []')
        all_data=$(echo "$all_data $page_data" | jq -s 'add')
        
        # Check for next page
        page_cursor=$(echo "$response" | jq -r '.next_page // empty')
        if [ -z "$page_cursor" ] || [ "$page_cursor" = "null" ]; then
            break
        fi
    done
    
    echo "$all_data"
}

# Fetch usage data
COMPLETIONS_DATA=$(fetch_usage_data "https://api.openai.com/v1/organization/usage/completions")
EMBEDDINGS_DATA=$(fetch_usage_data "https://api.openai.com/v1/organization/usage/embeddings")

# Combine usage data
USAGE_DATA=$(echo "$COMPLETIONS_DATA $EMBEDDINGS_DATA" | jq -s 'add')

# Fetch costs data
COSTS_DATA=$(fetch_costs_data)

# Check for errors
if [ -z "$USAGE_DATA" ] || [ "$USAGE_DATA" = "[]" ]; then
    echo "⚠️ OpenAI | color=orange"
    echo "---"
    echo "No usage data available | color=orange"
    echo "Period: $START_DATE to $END_DATE"
    exit 0
fi

# Aggregate usage statistics
TOTAL_INPUT_TOKENS=$(echo "$USAGE_DATA" | jq '[.[].results[]?.input_tokens // 0] | add // 0')
TOTAL_OUTPUT_TOKENS=$(echo "$USAGE_DATA" | jq '[.[].results[]?.output_tokens // 0] | add // 0')
TOTAL_TOKENS=$(echo "$TOTAL_INPUT_TOKENS $TOTAL_OUTPUT_TOKENS" | awk '{print $1 + $2}')
TOTAL_REQUESTS=$(echo "$USAGE_DATA" | jq '[.[].results[]?.num_model_requests // 0] | add // 0')

# Aggregate costs
TOTAL_COST=$(echo "$COSTS_DATA" | jq '[.[].results[]?.amount.value // 0 | tonumber] | add // 0')

# Format numbers for display
COST_FORMATTED=$(printf "%.2f" $TOTAL_COST)
TOKENS_FORMATTED=$(printf "%'d" $TOTAL_TOKENS)
INPUT_FORMATTED=$(printf "%'d" $TOTAL_INPUT_TOKENS)
OUTPUT_FORMATTED=$(printf "%'d" $TOTAL_OUTPUT_TOKENS)
REQUESTS_FORMATTED=$(printf "%'d" $TOTAL_REQUESTS)

# Determine color based on cost (optional: you can adjust thresholds)
if (( $(echo "$TOTAL_COST > 25" | bc -l) )); then
    COST_COLOR="red"
elif (( $(echo "$TOTAL_COST > 10" | bc -l) )); then
    COST_COLOR="orange"
else
    COST_COLOR="green"
fi

# Menu bar title (first line before ---)
echo "🤑 \$$COST_FORMATTED | color=$COST_COLOR"

# Dropdown menu (after ---)
# Note: SwiftBar may have limited color support in dropdown menus
# Using hex colors for better compatibility
echo "---"
echo "OpenAI API Usage & Costs | font=Monaco-Bold size=13 color=#000000"
echo "---"
echo "Period: $START_DATE to $END_DATE | font=Monaco size=12 color=#000000"
echo "---"
echo "Usage | font=Monaco-Bold size=13 color=#000000"
echo "Total Tokens: $TOKENS_FORMATTED | font=Monaco size=12 color=#000000"
echo "  Input:  $INPUT_FORMATTED | font=Monaco size=11 color=#000000"
echo "  Output: $OUTPUT_FORMATTED | font=Monaco size=11 color=#000000"
echo "Total Requests: $REQUESTS_FORMATTED | font=Monaco size=12 color=#000000"
echo "---"
echo "Costs | font=Monaco-Bold size=13 color=#000000"
echo "\$$COST_FORMATTED | color=$COST_COLOR font=Monaco-Bold size=14"
echo "---"
echo "Open Dashboard | href=https://platform.openai.com/usage font=Monaco size=11 color=#000000"
echo "Refresh | refresh=true font=Monaco size=11 color=#000000"

