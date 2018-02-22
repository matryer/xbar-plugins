#!/usr/bin/env ruby

# <bitbar.title>Whirlpool watched threads checker</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Tristan Harmer</bitbar.author>
# <bitbar.author.github>gondalez</bitbar.author.github>
# <bitbar.desc>List unread threads from your watch list</bitbar.desc>
# <bitbar.dependencies>ruby</bitbar.dependencies>

require 'json'
require 'time'
require 'net/https'

API_KEY = 'YOUR_KEY_HERE' # see https://whirlpool.net.au/profile/?action=account

uri = URI.parse("https://whirlpool.net.au/api/?key=#{API_KEY}&get=watched&watchedmode=0&output=json")

res = Net::HTTP.start(uri.hostname, uri.port, use_ssl: true, verify_mode: OpenSSL::SSL::VERIFY_NONE) do |http|
  req = Net::HTTP::Get.new(uri)
  http.request(req)
end

puts 'http error' and exit unless res.is_a? Net::HTTPSuccess

watched = JSON.parse(res.body)['WATCHED']

puts ":cyclone: #{watched.length}|color=##{watched.length > 0 ? '000000' : 'cccccc'}"

puts '---'
watched.each do |thread|
  last_post_time = Time.parse(thread['LAST_DATE']).getlocal
  last_post_time_minutes = ((Time.now - last_post_time) / 60).round
  href = "https://forums.whirlpool.net.au/forum-replies.cfm?t=#{thread['ID']}&p=#{thread['LASTPAGE']}&#r#{thread['LASTREAD']}"
  puts "#{thread['TITLE']} -- #{last_post_time_minutes} mins ago | href=#{href}"
end
