#!/usr/bin/ruby

# <bitbar.title>Flag of the Charles</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Isaac</bitbar.author>
# <bitbar.author.github>irstacks</bitbar.author.github>
# <bitbar.desc>Shows Boston's current Community Boating, Inc flag and related weather information.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/gQ3suo0.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>

require 'nokogiri'
require 'open-uri'
require 'openssl'

# SPARK='/usr/local/Cellar/spark/1.0.1/bin/spark'

# We'll provide a link to this. 
CBI_WEATHER_PAGE = 'http://www.community-boating.org/about-us/weather-information/'

# Grab weather statistics from here (wind speed, etc)
HOBO_WEATHER_PAGE = 'https://www.hobolink.com/p/0cdac4a6910cef5a8883deb005d73ae1'

# This is apparently where they put a js one-liners with their current flag status. 
CBI_FLAG_JS_PAGE = 'https://portal2.community-boating.org/pls/apex/CBI_PROD.FLAG_JS'

# Got an error once: 'Back-end server at capacity', which would be a 503(?). 
# However, visiting the page in the browser didn't generate an error, and once I did, the plugin worked again.
# Cookie thingey? SSL thingey? 
# The below is a hacky attempt at avoiding or at least better reporting the error.
flag_js_page = open(CBI_FLAG_JS_PAGE)
if flag_js_page.status[0] == '200'
	cbi_html = Nokogiri.HTML(flag_js_page)
else 
	flag_js_page_no_ssl = open(CBI_FLAG_JS_PAGE, {ssl_verify_mode: OpenSSL::SSL::VERIFY_NONE})
	if flag_js_page_no_ssl.status[0] == '200'
		cbi_html = Nokogiri.HTML(flag_js_page_no_ssl)
	else
		puts "! | color=red"
		puts "---"
		puts "#{flag_js_page}"
	end
end

## Get the flag color. ⚑!
flag_elem = cbi_html.css('body').text
	# => var FLAG_COLOR='c';

# First, et only the actual single-letter var, ie c, G, Y, R
FLAG_VAR = flag_elem.scan(/"([^"]*)"/)[0][0]
# puts "#{FLAG_VAR}"

# This is how CBI decides what color the flag is. We'll do the same. 
# <script type="text/javascript">switch(FLAG_COLOR){case"G":document.write("<img src='http://www.community-boating.org/wp-content/themes/communityboating/images/green-flag.png' width='32' height='35' alt='green flag'> ");break;case"Y":document.write("<img src='http://www.community-boating.org/wp-content/themes/communityboating/images/yellow-flag.png' width='32' height='35' alt='yellow flag'> ");break;case"R":document.write("<img src='http://www.community-boating.org/wp-content/themes/communityboating/images/red-flag.png' width='32' height='35' alt='red flag'> ");break;default:document.write("<a href='http://www.community-boating.org/about-us/weather-information/'><img src='http://www.community-boating.org/wp-content/themes/communityboating/images/burgee.png' id='cbi-burgee' width='45' height='31'></a> ");}</script>
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
hobo_weather_html = Nokogiri.HTML(open(HOBO_WEATHER_PAGE))

stats = {}
# => {"Wind Speed:"=>"3.4", "Gust Speed:"=>"10.2", "Wind Direction:"=>"NNE 20", "Rain:"=>"0.00", "Air Temp:"=>"48.41", "Water Temp:"=>"52.86", "Pressure:"=>"29.880", "PAR:"=>"384", "RH:"=>"99.00", "Dew Point:"=>"48.15", "Battery:"=>"4.415"}

units = {}
# => {"Wind Speed:"=>"mph", "Gust Speed:"=>"mph", "Wind Direction:"=>"°", "Rain:"=>"in", "Air Temp:"=>"°F", "Water Temp:"=>"°F", "Pressure:"=>"inHg", "PAR:"=>"uE", "RH:"=>"%", "Dew Point:"=>"°F", "Battery:"=>"V"}

hobo_weather_html.css('.latest-conditions-info').each do |con|
	label = con.css('span.latest-conditions-info-label')[0].text
	reading = con.css('span.latest-conditions-info-reading')[0].text
	unit_value = con.css('span.latest-conditions-info-units')[0].text
	
	stats[label] = reading
	units[label] = unit_value
end

# sparky = %x(/usr/local/Cellar/spark/1.0.1/bin/spark "#{stats['Wind Speed:']}" "#{stats['Gust Speed:']}").to_s.strip! # return string


## Put put. 

puts "#{ICON_SHAPE} | color=#{COLOR}"
puts '---'
puts "#{stats['Wind Speed:']}/#{stats['Gust Speed:']}#{units['Wind Speed:']} ☇ #{stats['Wind Direction:']}" # #{sparky} 
puts "Rain: #{stats['Rain:']} in"
puts "Air Temp: #{stats['Air Temp:']} F"
puts "Water Temp: #{stats['Water Temp:']} F"
puts '---'
puts "CBI Weather Information | href=#{CBI_WEATHER_PAGE}"


# Some relevant(?) icons.
# ☁︎☂☔︎☀︎☼☇⚑⚐♒︎☇☈
 
