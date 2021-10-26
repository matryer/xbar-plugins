#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

# <xbar.title>Average Review Times</xbar.title>
# <xbar.version>v0.1.2</xbar.version>
# <xbar.author>mfks17</xbar.author>
# <xbar.author.github>mfks17</xbar.author.github>
# <xbar.desc>Average App Store Review Times</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/mfks17/bitbar-plugin-AppStore/master/Screenshots/01.png</xbar.image>
# <xbar.dependencies>ruby, nokogiri</xbar.dependencies>
# <xbar.abouturl>https://github.com/mfks17/bitbar-plugin-AppStore</xbar.abouturl>

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
