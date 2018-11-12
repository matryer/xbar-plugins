#!/usr/bin/env ruby
# encoding: utf-8

# <bitbar.title>Show Time machine Progress</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Slamet Kristanto</bitbar.author>
# <bitbar.author.github>drselump14</bitbar.author.github>
# <bitbar.desc>Show time machine backup progress</bitbar.desc>
# <bitbar.dependencies>time machine</bitbar.dependencies>

require 'date'

status_lines = `tmutil status`
status = {}
status_lines.each_line do |line|
  status[:percent] = (line.split("=")[1][2..-4].strip.to_f * 100).round() if /^ *Percent =/ =~ line
  status[:running] = (line.split("=")[1][0..-2].strip).to_i if /^ *Running =/ =~ line
  status[:stopping] = (line.split("=")[1][0..-2].strip).to_i if /^ *Stopping =/ =~ line
  status[:time_remaining] = (line.split("=")[1][0..-2].strip).to_i if /^ *TimeRemaining =/ =~ line
  status[:cur_bytes] = (line.split("=")[1][0..-2].strip).to_i if /^ *bytes =/ =~ line
  status[:total_bytes] = (line.split("=")[1][0..-2].strip).to_i if /^ *totalBytes =/ =~ line
  status[:backup_phase] = line.split("=")[1][0..-2].strip if /^ *BackupPhase =/ =~ line
end

if status[:running] == 0
  status_line = ""
elsif status[:stopping] == 1
  status_line = "Stopping"
else
  if status[:percent] == -100
    status_line = "Preparing"
  else
    if status[:backup_phase].start_with?("Copying")
      status_line = "#{status[:percent]}%"
    else
      status_line = "Completing"
    end
  end
end

if File.exist?('/private/var/db/.TimeMachine.Results')
  last_backup = DateTime.strptime((`defaults read /private/var/db/.TimeMachine.Results BACKUP_COMPLETED_DATE`.strip), "%Y-%m-%d %H:%M:%S %z").new_offset(DateTime.now.offset).strftime("%A %B %d, %Y %l:%M %P")
else
  last_backup = `/usr/libexec/PlistBuddy -c "Print Destinations:0:SnapshotDates" /Library/Preferences/com.apple.TimeMachine.plist | tail -n 2 | head -n 1`.strip
end

last_backup = "last backup #{last_backup}"
unless status_line.empty?
  puts  "⏳#{status_line} "
  puts  "---"
  puts  "(#{status[:cur_bytes]/1000000000}/#{status[:total_bytes]/1000000000}GB) | size=11" unless status[:percent] == -100
  puts  "#{last_backup}"
end
