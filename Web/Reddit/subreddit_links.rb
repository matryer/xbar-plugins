#!/usr/bin/env ruby

# <bitbar.title>Subreddit Links</bitbar.title>
# <bitbar.author>Joe Canero</bitbar.author>
# <bitbar.author.github>caneroj1</bitbar.author.github>
# <bitbar.image>https://i.imgur.com/3ZDUdNH.png</bitbar.image>
# <bitbar.version>1.0</bitbar.version>

require 'net/http'
require 'json'

#### Tips ####
# Update the list of subreddits below
# to customize the functionality of this
# plugin.
# The format should be like this:
# [name_of_subreddit, "/top" or "/new"]
# If you instead want to get links from
# the front page instead of a subreddit,
# leave name_of_subreddit empty: ""
#### #### ####
SUBREDDITS = [
  ["",            "/top"],
  ["r/AskReddit", "/top"],
]

REDDIT = "https://www.reddit.com/"
def to_json(subreddit, type)
  url = "#{REDDIT}#{subreddit}#{type}.json"
  data = JSON.parse(Net::HTTP.get(URI(url)))
  data["subreddit_name"] = (subreddit.eql?("") ? "Front Page" : subreddit) + type
  data
end

def prettify(json)
  puts "---"
  if json["quarantine"] || json["over_18"]
    puts "NSFW | color=red"
  end

  puts json["title"] + " | color=#337ab7 | href=#{REDDIT}" + json["permalink"]
  puts "Score: #{json["score"]}, Comments: #{json["num_comments"]}"
end


begin
  puts "Reddit\n---"
  SUBREDDITS.map { |subreddit| to_json(*subreddit) }
            .each do |data|
              puts "\n---\n#{data["subreddit_name"]} | color=black\n---"
              data["data"]["children"].each { |child| prettify(child["data"]) }
            end
rescue => e
  puts e
  puts "Content is currently unavailable. Please try resetting. | color=red"
end
