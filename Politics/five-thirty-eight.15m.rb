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

page = Nokogiri::HTML(open("http://projects.fivethirtyeight.com/2016-election-forecast/"))
feed = Nokogiri::XML(open("https://fivethirtyeight.com/tag/2016-election/feed/"))

d_pct = page.css('.candidates.heads .candidate.dem .candidate-text .candidate-val.winprob')
          .first.text.gsub(/[^0-9.]/, '').to_f
r_pct = page.css('.candidates.heads .candidate.rep .candidate-text .candidate-val.winprob')
          .first.text.gsub(/[^0-9.]/, '').to_f

if d_pct > r_pct
  puts ":princess: #{d_pct}%"
else
  puts ":imp: #{r_pct}%"
end
puts "---"
puts "Chance of Winning:"
puts ":princess: Hillary Clinton: #{d_pct}%"
puts ":imp: Donald Trump: #{r_pct}%"

puts "---"
puts "Electoral Votes:"
page.css(".card-natl-tables .table.one .cand").each do |row|
  puts "#{row.css(".name.desktop").text}: #{row.css('.candidate-val').text}"
end

puts "---"
puts "Popular Vote:"
page.css(".card-natl-tables .table.two .cand").each do |row|
  puts "#{row.css(".name.desktop").text}: #{row.css('.candidate-val').text}"
end

puts "---"
puts ":wolf: FiveThirtyEight Election Forecast | href=http://projects.fivethirtyeight.com/2016-election-forecast/"
feed.css("item")[0..2].each do |item|
  date = Date::parse(item.css('pubDate').text)
  puts "#{date.strftime("%Y-%m-%d")}: #{item.css('title').text} | href=#{item.css('link').text}"
end

puts "---"
puts "Refresh... | refresh=true"
