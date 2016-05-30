#!/usr/bin/ruby

# <bitbar.title>Flag of the Charles</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Isaac</bitbar.author>
# <bitbar.author.github>irstacks</bitbar.author.github>
# <bitbar.desc>Shows Boston's current Community Boating, Inc flag and related weather information.</bitbar.desc>
# <bitbar.image>http://www.community-boating.org/wp-content/themes/communityboating/images/burgee.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>

require 'nokogiri'
require 'open-uri'


CBI_PAGE = 'http://www.community-boating.org/about-us/weather-information/'
HOBO_WEATHER_PAGE = 'https://www.hobolink.com/p/0cdac4a6910cef5a8883deb005d73ae1'

hobo_html = Nokogiri.HTML(open(HOBO_WEATHER_PAGE))
cbi_html = Nokogiri.HTML(open(CBI_PAGE))

## Get the flag color. ⚑!

# The CBI website generates their flag icons inline with some mysterious javascript.
# Luckily for us, they set the constant in a <script> in the <head>.
flag_elem = cbi_html.at('script:contains("var FLAG_COLOR")').text.strip
	# => var FLAG_COLOR='c';

# This is how CBI decides what color the flag is. We'll do the same. 
# <script type="text/javascript">switch(FLAG_COLOR){case"G":document.write("<img src='http://www.community-boating.org/wp-content/themes/communityboating/images/green-flag.png' width='32' height='35' alt='green flag'> ");break;case"Y":document.write("<img src='http://www.community-boating.org/wp-content/themes/communityboating/images/yellow-flag.png' width='32' height='35' alt='yellow flag'> ");break;case"R":document.write("<img src='http://www.community-boating.org/wp-content/themes/communityboating/images/red-flag.png' width='32' height='35' alt='red flag'> ");break;default:document.write("<a href='http://www.community-boating.org/about-us/weather-information/'><img src='http://www.community-boating.org/wp-content/themes/communityboating/images/burgee.png' id='cbi-burgee' width='45' height='31'></a> ");}</script>

# First, et only the actual single-letter var, ie c, G, Y, R
FLAG_VAR = flag_elem.scan(/'([^']*)'/)

case FLAG_VAR
when 'R'
	ICON_SHAPE = '⚑'
	COLOR = 'red'
when 'Y'
	ICON_SHAPE = '⚑'
	COLOR = 'yellow'
when 'G'
	ICON_SHAPE = '⚑'
	COLOR = 'green'
else 
	ICON_SHAPE = '⚐'
	COLOR = 'white'
end


## Get the weather. 

stats = {}
# => {"Wind Speed:"=>"3.4", "Gust Speed:"=>"10.2", "Wind Direction:"=>"NNE 20", "Rain:"=>"0.00", "Air Temp:"=>"48.41", "Water Temp:"=>"52.86", "Pressure:"=>"29.880", "PAR:"=>"384", "RH:"=>"99.00", "Dew Point:"=>"48.15", "Battery:"=>"4.415"}

units = {}
# => {"Wind Speed:"=>"mph", "Gust Speed:"=>"mph", "Wind Direction:"=>"°", "Rain:"=>"in", "Air Temp:"=>"°F", "Water Temp:"=>"°F", "Pressure:"=>"inHg", "PAR:"=>"uE", "RH:"=>"%", "Dew Point:"=>"°F", "Battery:"=>"V"}

hobo_html.css('.latest-conditions-info').each do |con|
	_label = con.css('span.latest-conditions-info-label')[0].text
	_reading = con.css('span.latest-conditions-info-reading')[0].text
	_units = con.css('span.latest-conditions-info-units')[0].text
	stats[_label] = _reading
	units[_label] = _units
end


## Put put. 

puts "#{ICON_SHAPE} | color=#{COLOR}"
puts '---'
puts "#{stats['Wind Speed:']}/#{stats['Gust Speed:']}#{units['Wind Speed:']} ☇ #{stats['Wind Direction:']}"
puts "Rain: #{stats['Rain:']} in"
puts "Air Temp: #{stats['Air Temp:']} F"
puts "Water Temp: #{stats['Water Temp:']} F"
puts '---'
puts "CBI Weather Information | href=#{CBI_PAGE}"


# Some relevant(?) icons.
# ☁︎☂☔︎☀︎☼☇⚑⚐♒︎☇☈
 