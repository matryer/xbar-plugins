#!/usr/bin/env ruby

# <bitbar.title>External IP country flag emoji</bitbar.title>
# <bitbar.version>v1.5 beta 1</bitbar.version>
# <bitbar.author>Bruce Steedman</bitbar.author>
# <bitbar.author.github>MatzFan</bitbar.author.github>
# <bitbar.desc>Displays country flag emoji - e.g. for VPN use</bitbar.desc>
# <bitbar.dependencies>OS X 10.11</bitbar.dependencies>

require 'open-uri'
require 'json'

begin
  cc = JSON.load(open('http://ip-api.com/json'))
  country_code = cc['countryCode'].chomp.split ''
  c1, c2 = *country_code.map { |c| (c.ord + 0x65).chr.force_encoding 'UTF-8' }
  puts "\xF0\x9F\x87#{c1}\xF0\x9F\x87#{c2}"
  puts "---"
  puts "Public IP : " + cc['query']
  puts "ISP : " + cc['isp']
rescue StandardError => err
  puts "🚩"
  puts "---"
  puts err.to_s
end
