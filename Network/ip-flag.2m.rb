#!/usr/bin/env ruby

# <xbar.title>External IP country flag emoji</xbar.title>
# <xbar.version>v1.5 beta 1</xbar.version>
# <xbar.author>Bruce Steedman</xbar.author>
# <xbar.author.github>MatzFan</xbar.author.github>
# <xbar.desc>Displays country flag emoji - e.g. for VPN use</xbar.desc>
# <xbar.dependencies>OS X 10.11</xbar.dependencies>

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
