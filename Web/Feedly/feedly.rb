#!/usr/bin/env ruby

# <bitbar.title>Feedly</bitbar.title>
# <bitbar.author>morizyun</bitbar.author>
# <bitbar.author.github>morizyun</bitbar.author.github>
# <bitbar.image>https://farm2.staticflickr.com/1541/25991545133_924f4a1a59_c.jpg</bitbar.image>

require 'net/http'
require 'json'

ARTICLE_LIMIT = 20.freeze

## PROCEDURE TO GET AUTH TOKEN ##
# https://blog.morizyun.com/blog/feedly-feed-api-script-bitbar/index.html
FEEDLY_AUTH_TOKEN = 'YOUR access_token'.freeze

def get_json_by_feedly_api(url)
  uri = URI(url)

  req = Net::HTTP::Get.new(uri)

  req['Authorization'] = "Bearer #{FEEDLY_AUTH_TOKEN}"

  res = Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) { |http| http.request(req) }
  JSON.parse(res.body)
end

def feedly_feeds
  # get profile
  profile = get_json_by_feedly_api('https://cloud.feedly.com/v3/profile')

  # get feed information in your profile
  url = "https://cloud.feedly.com/v3/streams/contents?streamId=user/#{profile['id']}/category/global.all"
  content = get_json_by_feedly_api(url)

  return content['items']
end

def output(item)
  puts "#{item['title']} | href=#{item['originId']}"
rescue => e
  puts "An error occured: #{e}"
end

puts 'Feedly'
puts '---'
begin
  feedly_feeds.each { |item| output(item) }
rescue
  puts 'Content is currently unavailable. Please try resetting. | color=red'
end
