#!/usr/bin/env ruby

# <bitbar.title>Your time on earth</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Tim Regener</bitbar.author>
# <bitbar.author.github>timlapluie</bitbar.author.github>
# <bitbar.desc>Displays the time you are already living.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/EzUARsL.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>

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
