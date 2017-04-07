#!/usr/bin/env LC_ALL=en_US.UTF-8 ruby

# <bitbar.title>Worktime Tracker</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Ash Wu(hSATAC)</bitbar.author>
# <bitbar.author.github>hSATAC</bitbar.author.github>
# <bitbar.desc>A simple worktime tracker.</bitbar.desc>
# <bitbar.image>http://ash.cat/wGsi+</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>

# Functionality:
# * Start / stop / pause / resume the timing session.
# * Keep history records and export to a txt file.
# * Rename session.

require 'json'

### HELPERS ###

def prompt(question, default)
    result = `/usr/bin/osascript -e 'Tell application "System Events" to display dialog "#{question}" default answer "#{default}"' -e 'text returned of result' 2>/dev/null`.strip
    result.empty? ? defult : result
end

def notification(msg, title)
  `/usr/bin/osascript -e 'display notification "#{msg}" with title "#{title}"'`
end

### CLASSES ###

class WorkSession
  attr_reader :start_time, :end_time, :pause_time, :resume_time, :saved_duration, :name, :state

  def initialize(name)
    @name = name
    @state = "stopped"
    @saved_duration = 0
  end

  def duration
    t = @saved_duration

    if @state != "paused"
      if @resume_time
        t += (Time.now - @resume_time)
      else
        t += (Time.now - @start_time)
      end
    end

    Time.at(t).utc.strftime("%H:%M:%S")
  end

  def start
    return if @state != "stopped"
    @state = "recording"
    @start_time = Time.now
  end

  def stop
    return if @state != "recording"
    @state = "stopped"
    @end_time = Time.now
  end

  def pause
    return if @state != "recording"
    @state = "paused"
    @pause_time = Time.now
    @saved_duration += (@pause_time - @start_time)
  end

  def resume
    return if @state != "paused"
    @state = "recording"
    @resume_time = Time.now
  end

  def rename(new_name)
    @name = new_name
  end

  def to_s
    "#{@name}, #{@start_time} ~ #{@end_time}, #{duration}"
  end
end

class WorkTimer
  attr_reader :workdir, :session

  def initialize(workdir = nil)
    @workdir = workdir || File.dirname(__FILE__)
    load_session
  end

  def session_file
    File.join @workdir, ".worktimer.dat"
  end

  def history_file
    File.join @workdir, "worktimer", "history.txt"
  end

  def state
    @session.nil? ? "stopped" : @session.state
  end

  def start
    session_name = prompt("Enter session name:", "Unnamed Session")
    @session = WorkSession.new session_name
    @session.start
    save_session

    notification("New session [#{@session.name}] has started.", "Worktime Tracker")
  end

  def stop
    @session.stop
    save_history
    delete_session

    notification("You spent #{duration} in [#{@session.name}]", "Worktime Tracker")
  end

  def pause
    @session.pause
    save_session

    notification("Session [#{@session.name}] has been paused.", "Worktime Tracker")
  end

  def resume
    @session.resume
    save_session

    notification("Session [#{@session.name}] has been resumed.", "Worktime Tracker")
  end

  def rename
    abort "Session does not exist." if @session.nil?
    session_name = prompt("Enter new session name:", @session.name)
    @session.rename session_name
    save_session

    notification("Current session renamed to: [#{@session.name}]", "Worktime Tracker")
  end

  def duration
    @session.nil? ? '' : @session.duration
  end

  def save_session
    File.open(session_file, 'w') {|f| f.write(Marshal.dump(@session)) }
  end

  def delete_session
    File.delete(session_file)
  end

  def load_session
    if File.exists? session_file
      @session = Marshal.load(File.read(session_file))
    end
  end

  def save_history
    system 'mkdir', '-p', File.dirname(history_file)
    File.open(history_file, 'a') { |f| f.write("#{session.to_s}\n") }
  end

  def history
    if File.exists? history_file
      system '/usr/bin/open', history_file
    else
      notification("History file not found.", "Worktime Tracker")
    end
  end
end

timer = WorkTimer.new

### ACTIONS ###

if ARGV[0]
  action = ARGV[0].to_sym
  if timer.respond_to? action
    timer.send(action)
    exit
  end
end

### RENDER ###

REFRESH = "---\nReload| refresh=true"

case timer.state
when "stopped"
  TITLE = "▷"
  MENU = """
Start | bash='#{__FILE__}' param1=start terminal=false
"""
when "recording"
  TITLE = "◎" + " #{timer.duration}"
  MENU = """
#{timer.session.name} | bash='#{__FILE__}' param1=rename terminal=false
---
Pause | bash='#{__FILE__}' param1=pause terminal=false
Stop | bash='#{__FILE__}' param1=stop terminal=false
"""
when "paused"
  TITLE = "▣" + " #{timer.session.name} #{timer.duration}"
  MENU = """
#{timer.session.name} | bash='#{__FILE__}' param1=rename terminal=false
---
Resume | bash='#{__FILE__}' param1=resume terminal=false
"""
end

HISTORY = "History | bash='#{__FILE__}' param1=history terminal=false"

puts """
#{TITLE}
---
#{MENU}
---
#{HISTORY}
---
#{REFRESH}
"""

