#!/bin/bash
# <xbar.title>Workday Timer</xbar.title>
# <xbar.version>v2.0</xbar.version>
# <xbar.author>Alexandre Prates</xbar.author>
# <xbar.author.github>alexandreprates</xbar.author.github>
# <xbar.desc>9-hour workday timer with daily CSV logging, completion notification, and screen sleep prevention via caffeinate.</xbar.desc>
# <xbar.dependencies>bash,osascript</xbar.dependencies>

STATE_DIR="$HOME/.workday_timer"
STATE_FILE="$STATE_DIR/state"
LOG_FILE="$STATE_DIR/work_log.csv"
DURATION_SECONDS=$((9 * 60 * 60))

mkdir -p "$STATE_DIR"

now_epoch() {
  date +%s
}

now_date() {
  date +"%Y-%m-%d"
}

now_time() {
  date +"%H:%M:%S"
}

load_state() {
  if [ -f "$STATE_FILE" ]; then
    # shellcheck disable=SC1090
    source "$STATE_FILE"
  fi

  STATUS="${STATUS:-idle}"
  START_TS="${START_TS:-}"
  START_DATE="${START_DATE:-}"
  START_TIME="${START_TIME:-}"
  CAFFEINATE_PID="${CAFFEINATE_PID:-}"
}

save_state() {
  cat > "$STATE_FILE" <<EOF
STATUS="$STATUS"
START_TS="$START_TS"
START_DATE="$START_DATE"
START_TIME="$START_TIME"
CAFFEINATE_PID="$CAFFEINATE_PID"
EOF
}

ensure_log_file() {
  if [ ! -f "$LOG_FILE" ]; then
    echo "date,start,end" > "$LOG_FILE"
  fi
}

format_hms() {
  local total=$1
  local h=$((total / 3600))
  local m=$(((total % 3600) / 60))
  printf "%02d:%02d" "$h" "$m"
}

notify_done() {
  osascript -e 'display notification "Your workday has ended." with title "Workday completed" sound name "Glass"'
}

notify_start() {
  local expected_end=$1
  local expected_end_time
  expected_end_time="$(date -r "$expected_end" +"%H:%M:%S")"
  osascript -e "display notification \"Expected end: ${expected_end_time}\" with title \"Workday started\" sound name \"Glass\""
}

start_caffeinate() {
  if [ -n "$CAFFEINATE_PID" ] && kill -0 "$CAFFEINATE_PID" 2>/dev/null; then
    return
  fi

  # -d prevents display sleep
  # -i prevents idle sleep
  # -m prevents disk sleep
  caffeinate -d -i -m &
  CAFFEINATE_PID=$!
}

stop_caffeinate() {
  if [ -n "$CAFFEINATE_PID" ] && kill -0 "$CAFFEINATE_PID" 2>/dev/null; then
    kill "$CAFFEINATE_PID" 2>/dev/null
    wait "$CAFFEINATE_PID" 2>/dev/null
  fi
  CAFFEINATE_PID=""
}

write_log_entry() {
  ensure_log_file
  local end_time
  end_time="$(date -r "$1" +"%H:%M:%S")"

  # The log is organized by day with date, start, and end,
  # so it uses the workday start date.
  echo "${START_DATE},${START_TIME},${end_time}" >> "$LOG_FILE"
}

start_workday() {
  load_state
  local expected_end

  if [ "$STATUS" = "running" ] && [ -n "$START_TS" ]; then
    start_caffeinate
    save_state
    return
  fi

  STATUS="running"
  START_TS="$(now_epoch)"
  START_DATE="$(now_date)"
  START_TIME="$(now_time)"
  expected_end=$((START_TS + DURATION_SECONDS))
  start_caffeinate
  save_state
  notify_start "$expected_end"
}

reset_workday() {
  load_state
  stop_caffeinate
  STATUS="idle"
  START_TS=""
  START_DATE=""
  START_TIME=""
  save_state
}

finish_workday() {
  load_state

  if [ "$STATUS" = "running" ] && [ -n "$START_TS" ]; then
    local end_ts
    end_ts="$(now_epoch)"
    write_log_entry "$end_ts"
  fi

  stop_caffeinate
  notify_done

  STATUS="idle"
  START_TS=""
  START_DATE=""
  START_TIME=""
  save_state
}

handle_actions() {
  case "${1:-}" in
    start)
      start_workday
      ;;
    reset)
      reset_workday
      ;;
    open_log)
      ensure_log_file
      open "$LOG_FILE"
      ;;
    open_state_dir)
      open "$STATE_DIR"
      ;;
  esac
}

render_menu() {
  load_state

  if [ "$STATUS" = "running" ] && [ -n "$START_TS" ]; then
    start_caffeinate

    local now elapsed remaining expected_end
    now="$(now_epoch)"
    elapsed=$((now - START_TS))
    remaining=$((DURATION_SECONDS - elapsed))
    expected_end=$((START_TS + DURATION_SECONDS))

    if [ "$remaining" -le 0 ]; then
      finish_workday
      load_state

      echo "😴 "
      echo "---"
      echo "Start workday | bash=\"$0\" param1=start terminal=false refresh=true"
      echo "Open log | bash=\"$0\" param1=open_log terminal=false"
      echo "Open data folder | bash=\"$0\" param1=open_state_dir terminal=false"
      return
    fi

    echo "🧑🏻‍💻 $(format_hms "$remaining")"
    echo "---"
    echo "Start: $START_TIME"
    echo "Expected end: $(date -r "$expected_end" +"%H:%M:%S")"
    echo "Screen sleep: disabled"
    echo "---"
    echo "Reset workday | bash=\"$0\" param1=reset terminal=false refresh=true"
    echo "Open log | bash=\"$0\" param1=open_log terminal=false"
    echo "Open data folder | bash=\"$0\" param1=open_state_dir terminal=false"
    return
  fi

  echo "😴 "
  echo "---"
  echo "Start workday | bash=\"$0\" param1=start terminal=false refresh=true"
  echo "Open log | bash=\"$0\" param1=open_log terminal=false"
  echo "Open data folder | bash=\"$0\" param1=open_state_dir terminal=false"
}

handle_actions "${1:-}"
render_menu
