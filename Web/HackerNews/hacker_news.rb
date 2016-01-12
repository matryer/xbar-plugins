#!/usr/bin/env ruby

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
  puts story["title"] + " | color=#337ab7 | href=" + story["url"]
  puts "Comments: #{story["descendants"]} | href=https://news.ycombinator.com/item?id=#{story["id"]} | color=black"
  puts "Score: #{story["score"]} | color=#{interpolate(story["score"])}"
  puts "---"
end

puts "HackerNews"
puts "---"
begin
  get_top_stories.map { |id| get_story_for_id(id) }.each { |story| output(story) }
rescue => e
  puts "Content is currently unavailable. Please try resetting. | color=red"
end
