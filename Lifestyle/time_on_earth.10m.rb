#!/usr/bin/env ruby

# <xbar.title>Your time on earth</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Tim Regener</xbar.author>
# <xbar.author.github>timlapluie</xbar.author.github>
# <xbar.desc>Displays the time you are already living.</xbar.desc>
# <xbar.image>http://i.imgur.com/EzUARsL.png</xbar.image>
# <xbar.dependencies>ruby</xbar.dependencies>

# --------------------- #
# EDIT THESE VARIABLES. #
# --------------------- #

# Add your Birthday here
# Format: 'YYYY-MM-DD [hh:mm (optional)] [UTC Offset (optional)]'
BIRTHDAY = '1988-01-12 11:42 +01:00'

# -------------------------------------------------------- #
# DON'T EDIT BELOW HERE UNLESS YOU KNOW WHAT YOU'RE DOING. #
# -------------------------------------------------------- #
require 'date'

birth_time = DateTime.parse(BIRTHDAY)
time_now = DateTime.now
delta = time_now - birth_time

days = delta.to_i

hours = (delta * 24)
minutes = (hours % 1) * 60
seconds = (minutes % 1) * 60

def format_int(number)
  number.to_i.to_s.reverse.gsub(/(\d{3})(?=\d)/, '\\1,').reverse
end

puts "#{format_int(days)} days on 🌍"
puts '---'
puts "Impressive! That's #{format_int(hours)} hours, #{format_int(minutes)} minutes, #{format_int(seconds)} seconds."
