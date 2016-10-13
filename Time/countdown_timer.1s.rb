#!/usr/bin/ruby
# <bitbar.title>Countdown Timer</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Chris Yuen</bitbar.author>
# <bitbar.author.github>kizzx2</bitbar.author.github>
# <bitbar.desc>Simple countdown timer. Set the time by calling the script from terminal.</bitbar.desc>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.image>https://raw.githubusercontent.com/kizzx2/bitbar-countdown-timer/master/screenshot.png</bitbar.image>
# <bitbar.abouturl>http://github.com/kizzx2/bitbar-countdown-timer</bitbar.abouturl>

fn = File.join(File.dirname($0), '.countdown')

if ARGV.count == 0
  task = nil

  if File.file?(fn)
    lines = File.read(fn).lines

    time = Time.at(lines.first.to_i)
    task = lines[1] if lines.count > 1
  else
    time = Time.at(0)
  end

  remain = time - Time.now

  if remain.to_i == 0
    system %(osascript -e 'display notification "Times up!" with title "Times up!" sound name "Glass"')
  end

  remain = 0 if remain < 0

  color = nil

  if remain < 15 * 60 && remain != 0
    color = "red"
  elsif remain < 30 * 60 && remain != 0
    color = "orange"
  end

  h = (remain / 3600).to_i
  remain -= h * 3600

  m = (remain / 60).to_i
  remain -= m * 60

  s = remain

  str = ""
  str << "#{task}: " if task
  str << "%02i:%02i:%02i" % [h, m, s]
  str << "| color=#{color}" if color

  puts str
else
  case ARGV.first
  when '0'
    time = 0
  when /^(\d+)s$/
    time = Time.now + $1.to_i
  when /^(\d+)m$/
    time = Time.now + $1.to_i * 60
  when /^(\d+)h$/
    time = Time.now + $1.to_i * 3600
  else
    puts "Error: Invalid argument '#{ARGV.first}'"
    exit 1
  end

  str = ""
  str << time.to_i.to_s

  if ARGV.count > 1
    str << "\n"
    str << ARGV.drop(1).join(' ')
  end

  File.write(fn, str)
end
