#!/usr/bin/env ruby

# <xbar.title>Feedly</xbar.title>
# <xbar.author>morizyun</xbar.author>
# <xbar.author.github>morizyun</xbar.author.github>
# <xbar.image>https://farm2.staticflickr.com/1541/25991545133_924f4a1a59_c.jpg</xbar.image>
# <xbar.var>string(FEEDLY_ACCESS_TOKEN): Your personal Feedly access token</xbar.var>

require 'net/http'
require 'json'

ARTICLE_LIMIT = 20.freeze

## PROCEDURE TO GET AUTH TOKEN ##
# https://blog.morizyun.com/blog/feedly-feed-api-script-bitbar/index.html
ACCESS_TOKEN = ENV['FEEDLY_ACCESS_TOKEN'].freeze

def get_json_by_feedly_api(url)
  uri = URI(url)

  req = Net::HTTP::Get.new(uri)

  req['Authorization'] = "Bearer #{ACCESS_TOKEN}"

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
  puts "#{item['title'].sub('|', '-')} | href=#{item['originId']}"
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
