#!/usr/bin/env ruby
 # <bitbar.title>Election Tracker</bitbar.title>
 # <bitbar.version>v1.0</bitbar.version>
 # <bitbar.author>Chris Metcalf</bitbar.author>
 # <bitbar.author.github>chrismetcalf</bitbar.author.github>
 # <bitbar.desc>Scrapes election odds from FiveThirtyEight's election tracker</bitbar.desc>
 # <bitbar.image>http://i.imgur.com/1NeqVZ6.png</bitbar.image>
 # <bitbar.dependencies>ruby</bitbar.dependencies>
 # <bitbar.abouturl>https://github.com/chrismetcalf/bitbar-plugins</bitbar.abouturl>

require 'open-uri'
require 'nokogiri'
require 'csv'

feed = Nokogiri::XML(open("https://fivethirtyeight.com/tag/2020-election/feed/"))

table = []
projectDataLink = 'https://projects.fivethirtyeight.com/2020-general-data/presidential_national_toplines_2020.csv'
begin
  open(projectDataLink) do |f|
    table = CSV.parse(f, headers: true)
  end
rescue => e
  puts "⚠️⚠️"
  puts "something went wrong with this link: #{projectDataLink}"
return
end

d_pct = "%.1f" % (table[0]["ecwin_chal"].to_f*100.0)
r_pct = "%.1f" % (table[0]["ecwin_inc"].to_f*100.0)

puts d_pct > r_pct ? "😎 #{d_pct}%" : ":imp: #{r_pct}%"

democratName = "😎 #{table[0]["candidate_chal"]}"
republicanName = ":imp: #{table[0]["candidate_inc"]}"
thirdParty = table[0]["candidate_3rd"]
thirdPartyName = "🤦 #{thirdParty}"
puts "---"
puts "Chance of Winning:"
puts "#{democratName}: #{d_pct}%"
puts "#{republicanName}: #{r_pct}%"

puts "---"
puts "Electoral Votes:"
puts "#{democratName}: #{table[0]["ev_chal"].to_i}"
puts "#{republicanName}: #{table[0]["ev_inc"].to_i}"
if thirdParty.to_s.length > 0
  puts "#{thirdPartyName}: #{table[0]["ev_3rd"]}"
end
puts "💆 No majority: #{table[0]["ec_nomajority"]}"

puts "---"
puts "Popular Vote:"
puts "#{democratName}: #{"%.1f" % table[0]["national_voteshare_chal"].to_f}%"
puts "#{republicanName}: #{"%.1f" % table[0]["national_voteshare_inc"].to_f}%"
if thirdParty.to_s.length > 0
  puts "#{thirdPartyName}: #{"%.1f" % table[0]["national_voteshare_3rd"]}%"
end
puts "🤷 Other: #{"%.1f" % table[0]["nat_voteshare_other"].to_f}%"

puts "---"
puts "📜 FiveThirtyEight Election Feed"
feed.css("item")[0..2].each do |item|
  date = Date::parse(item.css('pubDate').text)
  puts "#{date.strftime("%Y-%m-%d")}: #{item.css('title').text} | href=#{item.css('link').text}"
end

puts ":wolf: Visit FiveThirtyEight Election Forecast | href=http://projects.fivethirtyeight.com/2020-election-forecast/"

puts "---"
puts "Refresh... | refresh=true"
