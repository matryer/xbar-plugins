#!/usr/bin/env ruby

# <xbar.title>Life Percentage</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Kerem Bozdaş</xbar.author>
# <xbar.author.github>krmbzds</xbar.author.github>
# <xbar.desc>Shows what percentage of life you have lived.</xbar.desc>
# <xbar.image>http://i.imgur.com/DbXIhiS.png</xbar.image>
# <xbar.dependencies>ruby</xbar.dependencies>

# BitBar Life Percentage plugin
# by Kerem Bozdaş (@krmbzds)

dob = Time.new(1990, 12, 31, 10, 30, 0)  # Date of birth
dod = Time.new(2080, 12, 31, 10, 30, 0)  # Date of death
now = Time.now()
percent = ((now - dob) / (dod - dob) * 100).round(2)
puts "❤ #{percent}"
