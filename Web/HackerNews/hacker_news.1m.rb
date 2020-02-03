#!/usr/bin/env ruby

# <bitbar.title>Hacker News</bitbar.title>
# <bitbar.author>Mehmet Cetin</bitbar.author>
# <bitbar.author.github>manorie</bitbar.author.github>
# <bitbar.image>http://i.imgur.com/JuanUqk.png</bitbar.image>
# <bitbar.dependencies>Ruby</bitbar.dependencies>
# <bitbar.desc>HackerRank news with changing titles and persistence..</bitbar.desc>

# ~~Briefly~~
# Stores fecthed ids & news data in a .txt file
# Checks file updated_at and refreshes/persists the news in store time out
# Follows x number of stories, sorts by popularity
# Changes title in every time interval t (60 seconds by default)

# Configurations
STORAGE_FILE = ENV['HOME'] + '/hacker_news_data.txt'
STORAGE_TIME_OUT = 600 # file refresh in seconds
NUMBER_OF_NEWS = 12
TITLE_LIMIT = 50 # character limit for title

require 'net/http'
require 'json'
require 'date'

def file_time_out?(file)
  (Time.now - file.mtime) > STORAGE_TIME_OUT
end

def refresh_store
  file = File.exist?(STORAGE_FILE) ? File.open(STORAGE_FILE, 'r') : File.new(STORAGE_FILE, 'w')
  fetch_remote if file.size == 0 || file_time_out?(file)
  file.close
end

def fetch_remote
  story_ids = fetch_ids
  stories = fetch_stories(story_ids).sort_by { |s| -1 * s[:score] }
  persist(stories)
end

def persist(stories)
  file = File.open(STORAGE_FILE, 'w')
  file.write(Marshal.dump(stories))
ensure
  file.close unless file.nil?
end

def fetch_stories(ids)
  story_hashes = []
  ids.each do |id|
    s = story_with_id(id)
    story_hashes << s unless s.nil?
  end
  story_hashes
end

def story_with_id(id)
  url = "https://hacker-news.firebaseio.com/v0/item/#{id}.json?"
  j = JSON.parse(Net::HTTP.get(URI(url)))
  { id: j['id'],
    url: j['url'],
    title: j['title'],
    score: j['score'],
    descendants: j['descendants'] }
rescue
  nil
end

def fetch_ids
  url = 'https://hacker-news.firebaseio.com/v0/topstories.json'
  JSON.parse(Net::HTTP.get(URI(url)))[0...NUMBER_OF_NEWS]
rescue
  sleep(20)
  fetch_ids
end

def fetch_store
  file = File.open(STORAGE_FILE, 'r')
  Marshal.load(file.read)
rescue Errno::ENOENT
  return []
ensure
  file.close unless file.nil?
end

def headline_news_no
  # change headline in every refresh
  # synchronize denominator with refresh interval of file
  # for instance: if file has 1m refresh time in title like *.1m.rb
  # unit time has to be divided by 60
  (Time.now.to_i / 60) % NUMBER_OF_NEWS
end

def print_title(title)
  if title.length > TITLE_LIMIT
    puts "🍊  #{title[0..TITLE_LIMIT]}..."
  else
    puts "🍊  #{title}"
  end
end

def print(s)
  puts "#{s[:title]} | href=#{s[:url]}"
  puts "Score: #{s[:score]} Comments: #{s[:descendants]} | href=#{'https://news.ycombinator.com/item?id=' + s[:id].to_s} color=#FF6600"
  puts '---'
end

begin
  refresh_store
  entries = fetch_store

  if entries.length > 0
    headline = entries[headline_news_no]
    print_title(headline[:title])
    puts '---'
  end

  entries.each { |e| print(e) }
rescue StandardError => msg
  puts 'Error occured, please refresh bitbar! >' + msg.to_s
end
