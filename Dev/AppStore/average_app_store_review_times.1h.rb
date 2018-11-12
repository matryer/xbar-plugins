#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

# <bitbar.title>Average Review Times</bitbar.title>
# <bitbar.version>v0.1.2</bitbar.version>
# <bitbar.author>mfks17</bitbar.author>
# <bitbar.author.github>mfks17</bitbar.author.github>
# <bitbar.desc>Average App Store Review Times</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/mfks17/bitbar-plugin-AppStore/master/Screenshots/01.png</bitbar.image>
# <bitbar.dependencies>ruby, nokogiri</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/mfks17/bitbar-plugin-AppStore</bitbar.abouturl>

require 'open-uri'
require 'nokogiri'

url = 'http://appreviewtimes.com/'

charset = nil
html = open(url) do |f|
  charset = f.charset
  f.read
end

doc = Nokogiri::HTML.parse(html, nil, charset)

puts 'Average Review Times'
puts '---'
puts '📱  iOS App Store ' + doc.xpath('/html/body/div[2]/div/div[2]/div[1]/p[1]').text

puts '💻  Mac App Store ' + doc.xpath('/html/body/div[2]/div/div[3]/div[1]/p[1]').text
