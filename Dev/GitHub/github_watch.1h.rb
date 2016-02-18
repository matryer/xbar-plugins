#!/usr/bin/env ruby

# <bitbar.title>GitHub Watch</bitbar.title>
# <bitbar.version>v0.1.0</bitbar.version>
# <bitbar.author>D. Khamsing</bitbar.author>
# <bitbar.author.github>dkhamsing</bitbar.author.github>
# <bitbar.desc>Show GitHub stars ⭐️ for a list of repos</bitbar.desc>
# <bitbar.image>http://i.imgur.com/z1qhSun.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/dkhamsing</bitbar.abouturl>

require 'open-uri'
require 'json'

REPOS = [
  'matryer/bitbar',
  'dkhamsing/awesome_bot'
]

VERSION = '0.1.0'

CONFIG_GITHUB_WATCH = 'stargazers_count'

CONFIG_SYMBOL = '★'

GITHUB_REPO_API = 'https://api.github.com/repos'

def get_stars(repos)
  s = []
  repos.each do |r|
    repo_url = "#{GITHUB_REPO_API}/#{r}"
    c = open repo_url
    j = JSON.parse c.read
    s.push j[CONFIG_GITHUB_WATCH]
  end
  s
end

def line(r, s)
  repo_url = "https://github.com/#{r}"
  puts "#{r} #{CONFIG_SYMBOL} #{s} | href=#{repo_url}"
end

# bitbar output
begin
get_stars(REPOS).each_with_index { |s, i| line REPOS[i], s.to_s }
rescue => e
  puts "#{CONFIG_SYMBOL} | color=red"
  puts "Error: #{e}"
  exit
end
