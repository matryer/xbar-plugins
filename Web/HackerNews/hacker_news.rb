#!/usr/bin/env ruby

# <bitbar.title>Hacker News</bitbar.title>
# <bitbar.author>Joe Canero</bitbar.author>
# <bitbar.author.github>caneroj1</bitbar.author.github>
# <bitbar.image>https://i.imgur.com/bghlATz.png</bitbar.image>

require 'net/http'
require 'json'

NUMBER_OF_STORIES = 5
MAX_SCORE = 300

def get_top_stories
  url = "https://hacker-news.firebaseio.com/v0/topstories.json"
  JSON.parse(Net::HTTP.get(URI(url)))[0...NUMBER_OF_STORIES]
end

def get_story_for_id(id)
  url = "https://hacker-news.firebaseio.com/v0/item/#{id}.json?"
  JSON.parse(Net::HTTP.get(URI(url)))
end

def conv(val)
  (val / MAX_SCORE.to_f * 255).to_i
end

def interpolate(score)
  red = conv(MAX_SCORE - [score, MAX_SCORE].min)
  green = conv([score, MAX_SCORE].min)
  "#%02X%02X%02X" % [red, green, 0]
end

def output(story)
  begin
    puts "#{story["title"]} | href=#{story["url"]} color=#337ab7"
    puts "Comments: #{story["descendants"]} | href=https://news.ycombinator.com/item?id=#{story["id"]} color=black"
    puts "Score: #{story["score"]} | color=#{interpolate(story["score"])}"
  rescue => exception
    puts "An error occured: " + exception.to_s
  end
  puts "---"
end

puts "HackerNews"
puts "---"
begin
  get_top_stories.map { |id| get_story_for_id(id) }.each { |story| output(story) }
rescue => _
  puts "Content is currently unavailable. Please try resetting. | color=red"
end
