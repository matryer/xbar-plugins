#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

# <bitbar.title>Github Trending</bitbar.title>
# <bitbar.version>v0.1.2</bitbar.version>
# <bitbar.author>mfks17</bitbar.author>
# <bitbar.author.github>mfks17</bitbar.author.github>
# <bitbar.desc>Github Daily Trending Viewer</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/mfks17/bitbar-plugin-github-trending/Screenshots/01.png</bitbar.image>
# <bitbar.dependencies>ruby, nokogiri</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/mfks17/bitbar-plugins-github-trending</bitbar.abouturl>

require 'open-uri'
require 'json'
require 'nokogiri'

LANG = 'swift'.freeze # your favorite language
# LANG = 'java'.freeze

url = 'https://github.com/trending/' + LANG + '?since=daily'
BASE_URL = 'https://github.com/'.freeze

charset = nil
html = open(url) do |f|
  charset = f.charset
  f.read
end

hash = {}

puts LANG.capitalize
puts '---'

doc = Nokogiri::HTML.parse(html, nil, charset)
doc.xpath('//li[@class="repo-list-item"]').each do |node|
  node.xpath('./h3[@class="repo-list-name"]/a').attribute('href').value.each_line do |s|
    s.slice!(0)
    hash = { name: s, url: BASE_URL + s }

    api = 'https://api.github.com/repos/' + s
    begin
      res = open(api)
      code, message = res.status
    rescue => _
      puts '🙅Github Api Limits🙅'
      exit
    end

    if code == '200'
      result = JSON.parse(res.read)
      puts hash.fetch(:name) + ' ⭐️ Daily: ' + node.xpath('./p[@class="repo-list-meta"]').text.split("\n")[5].split(' ')[0] + ' - Total: ' + result.fetch('stargazers_count').to_s + '| sizes=14 href=' + hash.fetch(:url)
    else
      puts "OMG!! #{code} #{message}"
    end
  end
end
