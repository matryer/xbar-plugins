#!/usr/bin/env ruby

# <bitbar.title>Life Percentage</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Kerem Bozdaş</bitbar.author>
# <bitbar.author.github>krmbzds</bitbar.author.github>
# <bitbar.desc>Shows what percentage of life you have lived.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/DbXIhiS.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>

# BitBar Life Percentage plugin
# by Kerem Bozdaş (@krmbzds)

dob = Time.new(1990, 12, 31, 10, 30, 0)  # Date of birth
dod = Time.new(2080, 12, 31, 10, 30, 0)  # Date of death
now = Time.now()
percent = ((now - dob) / (dod - dob) * 100).round(2)
puts "❤ #{percent}"
