#!/usr/bin/env ruby

# <xbar.title>Better MPD</xbar.title>
# <xbar.version>1.0</xbar.version>
# <xbar.author>bandithedoge</xbar.author>
# <xbar.author.github>bandithedoge</xbar.author.github>
# <xbar.desc>A more advanced MPD control plugin.</xbar.desc>
# <xbar.dependencies>ruby,ruby-mpd</xbar.dependencies>
# <xbar.image>https://i.imgur.com/53Sf3VO.png</xbar.image>

# <xbar.var>string(MPC_PATH="/usr/local/bin/mpc"): Path to mpc.</xbar.var>
# <xbar.var>number(TITLE_LENGTH=40): Maximum title length.</xbar.var>
# <xbar.var>number(LONG_TITLE_LENGTH=100): Maximum dropdown title length.</xbar.var>
# <xbar.var>string(PIPE_REPLACE="\"): What to replace the pipe (|) character in titles with. It breaks the output for xbar, therefore we need to remove it.</xbar.var>
#
# <xbar.var>string(PLAY_ICON="▶️"): Play icon.</xbar.var>
# <xbar.var>string(PAUSE_ICON="⏸"): Pause icon.</xbar.var>
# <xbar.var>string(STOP_ICON="⏹"): Stop icon.</xbar.var>
# <xbar.var>string(NEXT_ICON="⏭"): Next icon.</xbar.var>
# <xbar.var>string(PREV_ICON="⏮"): Previous icon.</xbar.var>
# <xbar.var>string(RANDOM_ICON="🔀"): Random (shuffle) icon.</xbar.var>
# <xbar.var>string(REPEAT_ICON="🔁"): Repeat icon.</xbar.var>
# <xbar.var>string(SINGLE_ICON="🔂"): Single repeat icon.</xbar.var>
# <xbar.var>string(CONSUME_ICON="🔃"): Consume icon.</xbar.var>

require 'ruby-mpd'

mpd = MPD.new
mpd.connect
status = mpd.status
song = mpd.current_song
long_title_length = Integer(ENV["LONG_TITLE_LENGTH"])

case status[:state]
when :play
  state_icon = ENV["PLAY_ICON"]
when :pause
  state_icon = ENV["PAUSE_ICON"]
when :stop
  state_icon = ENV["STOP_ICON"]
end

mpc = ENV["MPC_PATH"]

def depipe string
  return string.tr "|", ENV["PIPE_REPLACE"].to_s
end

if status[:state] != :stop
  puts "#{state_icon} " + "#{depipe song.artist} - #{depipe song.title} | length=#{Integer(ENV["TITLE_LENGTH"])}".force_encoding("ASCII-8BIT")

  puts "---"
  puts "#{depipe song.title} | size=20 | length=#{long_title_length}"
  puts "-- " + status[:audio][0].to_s + " Hz / " + status[:bitrate].to_s + " kbps / " + status[:audio][2].to_s + " ch"
  puts "-- #{song.file} | length=#{long_title_length}"

  puts "#{depipe song.artist} | size=16 | length=#{long_title_length}"
  puts "#{depipe song.album} | size=12 | length=#{long_title_length}"

  puts "---"

  if status[:nextsong] != nil
    next_song = mpd.song_with_id(status[:nextsongid])
    puts "Next up: #{depipe next_song.artist} - #{depipe next_song.title} | length=#{long_title_length}"
  end

else
  puts "#{state_icon} Stopped"
end

puts "---"

if status[:state] == :play
  puts "#{ENV["PAUSE_ICON"]} Pause | shell=#{mpc} | param1=toggle"
else
  puts "#{ENV["PLAY_ICON"]} Play | shell=#{mpc} | param1=toggle"
end
puts "#{ENV["NEXT_ICON"]} Next | shell=#{mpc} | param1=next"
puts "#{ENV["PREV_ICON"]} Previous | shell=#{mpc} | param1=prev"
puts "#{ENV["STOP_ICON"]} Stop | shell=#{mpc} | param1=stop"

puts "---"

status_icon = ""

if status[:random]
  status_icon << ENV["RANDOM_ICON"]
end
if status[:single]
  status_icon << ENV["SINGLE_ICON"]
end
if status[:repeat]
  status_icon << ENV["REPEAT_ICON"]
end
if status[:consume]
  status_icon << ENV["CONSUME_ICON"]
end

puts "Status: #{status_icon}"
puts "-- Random: " + status[:random].to_s + " | shell=#{mpc} | param1=random"
puts "-- Repeat: " + status[:repeat].to_s + " | shell=#{mpc} | param1=repeat"
puts "-- Single: " + status[:single].to_s + " | shell=#{mpc} | param1=single"
puts "-- Consume: " + status[:consume].to_s + " | shell=#{mpc} | param1=consume"

puts "---"
