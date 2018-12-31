#!/usr/bin/env ruby

# Simple Verse of the Day plugin.
#
# by Harry Löwen
#
# Shows current verse of the day + verse image
# Thanks to YouVersion API Devs!
#
# Feel free to customize version, language, colors, etc.

# metadata
# <bitbar.title>Verse of the day</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Harry Löwen</bitbar.author>
# <bitbar.author.github>harryloewen</bitbar.author.github>
# <bitbar.desc>Display verse of the day and verse image.</bitbar.desc>
# <bitbar.image>https://drive.google.com/uc?export=download&id=1dpt4wWamYbk_l49u8IAOVf66wA27cAi0</bitbar.image>
# <bitbar.dependencies>ruby<bitbar.dependencies>
# <bitbar.abouturl>https://github.com/harryloewen/bitbar-votd/</bitbar.abouturl>

require 'net/http'
require 'open-uri'
require 'json'
require 'openssl'
require 'Date'
require 'base64'

BIBLE_LOGO = 'https://www.bible.com/favicon.ico'.freeze
BASE_URL = 'https://developers.youversionapi.com/1.0/verse_of_the_day'.freeze
TODAY = Date.today.yday.to_s
TOKEN = '...'.freeze
LANG = 'en'.freeze
VERSION_ID = '1'.freeze # KJV: 1, ASV: 12, RVES: 147, WEB: 20, ...
COLOR = '#696969'.freeze

url = URI("#{BASE_URL}/#{TODAY}?version_id=#{VERSION_ID}")
http = Net::HTTP.new(url.host, url.port)
http.use_ssl = true
http.verify_mode = OpenSSL::SSL::VERIFY_NONE

request = Net::HTTP::Get.new(url)
request['content-type'] = 'application/json'
request['accept'] = 'application/json'
request['x-youversion-developer-token'] = TOKEN
request['accept-language'] = LANG

response = JSON.parse http.request(request).read_body

puts "\n---\nresponse: #{response['message']} | color=red" if response['message']

bible_logo = Base64.encode64(open(BIBLE_LOGO).read).delete("\n")
reference = response['verse']['human_reference']
verse = response['verse']['text'].split.each_slice(8).map{|a|a.join ' '}.join("| color=#{COLOR}\n")
verse_url = response['verse']['url']
image_url = 'https:' + response['image']['url'].gsub('{width}', '300').gsub('{height}', '300')
image_base64 = Base64.encode64(open(image_url).read).delete("\n")
image_attribution = response['image']['attribution']

puts "
#{reference} | image=#{bible_logo}
---
#{verse} | color=#{COLOR}
#{reference} | color=#{COLOR}
---
| image=#{image_base64}
#{image_attribution}
---
🔗  open in bible.com | href=#{verse_url}
---
Refresh ⟳| refresh=true
"
