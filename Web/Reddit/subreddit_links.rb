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

# Feel free to make the user agent your username
# It looks like reddit just requires the user agent to be
# something unique and not generic.
USER_AGENT = "bitbar-user-agent"
REDDIT = "https://www.reddit.com/"
DEFAULT_PORT = 443

def to_json(subreddit, type)
  uri = URI.parse("#{REDDIT}#{subreddit}#{type}.json")
  @http = Net::HTTP::Get.new(uri)
  @http.add_field('User-Agent', USER_AGENT)
 
  res = Net::HTTP.start(uri.host, DEFAULT_PORT, :use_ssl => true) do |http| 
    http.request(@http)
  end
 
  data = JSON.parse(res.body)
  data["subreddit_name"] = (subreddit.eql?("") ? "Front Page" : subreddit) + type
  data
end

def prettify(json)
  puts "---"
  if json["quarantine"] || json["over_18"]
    puts "NSFW | color=red"
  end

  puts json["title"] + "| href=#{REDDIT}" + json["permalink"] + " color=#337ab7"
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
