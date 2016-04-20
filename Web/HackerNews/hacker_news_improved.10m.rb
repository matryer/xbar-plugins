#!/usr/bin/env ruby

# <bitbar.title>Hacker News Improved</bitbar.title>
# <bitbar.author>Mehmet Cetin</bitbar.author>
# <bitbar.author.github>manorie</bitbar.author.github>
# <bitbar.image>http://i.imgur.com/BT1JJ6U.png</bitbar.image>
# <bitbar.dependencies>Ruby</bitbar.dependencies>
# <bitbar.desc>Fetch HackerRank news with less data usage and be safer..</bitbar.desc>

# number of stories to show
STORY_COUNT = 12

# data file to persist news
DATA_FILE = ENV['HOME'] + '/hacker_news_data.txt'

# time out for removing news record from data file and refresh the it
# default is 10 minutes
TIME_OUT_SECONDS = 600

# character limit of title to appear on bar
TITLE_LIMIT = 60

require 'net/http'
require 'json'
require 'date'

def store
  file = File.open(DATA_FILE, 'r')
  return Marshal.load(file.read)
# if file not present
rescue Errno::ENOENT
  return []
ensure
  file.close unless file.nil?
end

def write_to(store)
  file = File.open(DATA_FILE, 'w')
  file.write(Marshal.dump(store))
ensure
  file.close unless file.nil?
end

def story_ids
  url = 'https://hacker-news.firebaseio.com/v0/topstories.json'
  JSON.parse(Net::HTTP.get(URI(url)))[0...STORY_COUNT]
rescue
  sleep(10)
  story_ids
end

def get_story_for_id(id)
  url = "https://hacker-news.firebaseio.com/v0/item/#{id}.json?"
  JSON.parse(Net::HTTP.get(URI(url)))
rescue
end

def append_to_store(store, h)
  store << { id: h['id'],
             url: h['url'],
             title: h['title'],
             score: h['score'],
             descendants: h['descendants'],
             added: Time.now }
  store
end

def story_persisted?(store, id)
  store.each { |record| return true if record[:id] == id }
  false
end

def print(s)
  puts "#{s[:title]} | href=#{s[:url]}"
  puts "Score: #{s[:score]} Comments: #{s[:descendants]} Refreshed:#{elapsed(s) / 60}mins"
  puts '---'
end

def elapsed(story)
  (Time.now - story[:added]).round
end

def print_title(title)
  if title.length > TITLE_LIMIT
    puts "#{title[0..TITLE_LIMIT]}..."
  else
    puts title
  end
end

begin
  data_store = store

  story_ids.each do |id|
    unless story_persisted?(data_store, id)
      data_store = append_to_store(data_store, get_story_for_id(id))
    end
  end

  data_store.each_with_index do |s, i|
    data_store.delete_at(i) if elapsed(s) > TIME_OUT_SECONDS
  end

  write_to(data_store)
  sorted = data_store.sort_by { |s| s[:score] }

  print_title(sorted.last[:title])
  puts '---'
  sorted.reverse_each { |story| print(story) }
rescue
  puts 'An error occured please refresh Bitbar.'
end
