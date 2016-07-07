#!/usr/bin/env ruby
#
# <bitbar.title>BitBar Version</bitbar.title>
# <bitbar.version>v0.2.0</bitbar.version>
# <bitbar.author>Olivier Tille</bitbar.author>
# <bitbar.author.github>oliviernt</bitbar.author.github>
# <bitbar.image>http://i.imgur.com/9BrFhSJ.png</bitbar.image>
# <bitbar.desc>Checks the current BitBar version against the latest from GitHub</bitbar.desc>
# <bitbar.dependencies>Ruby</bitbar.dependencies>
#
# BitBar version plugin
# by Olivier Tille (@oliviernt)
#
# Checks the current BitBar version against the latest from GitHub

require 'net/http'
require 'json'
require 'nokogiri'

# if you're seeing errors saying you've reached the API request limit you will need to
# create a new application at https://github.com/settings/developers and add client_id and client_secret here:
# GITHUB_CLIENT_ID=""
# GITHUB_CLIENT_SECRET=""
#
# then add the following query params to the github URL
# ?client_id=#{GITHUB_CLIENT_ID}&client_secret=#{GITHUB_CLIENT_SECRET}

def get_json
  url = URI("https://api.github.com/repos/matryer/bitbar/releases/latest")
  json_result = nil
  sleep_time = 0.5
  sleep_until = 16 # sleep 5x (0,5 * 2 ^ 5 == 16)
  while json_result.nil?
    begin
      json_result = JSON.parse(Net::HTTP.get(url))
    rescue => e
      sleep_time *= 2
      if sleep_time < sleep_until
        sleep sleep_time
      else
        throw e
      end
    end
  end
  json_result
end

def get_xml
  bitbar_path = `osascript -e 'tell application "System Events" to POSIX path of (file of process "BitBar" as alias)'`.chomp
  bitbar_path += "/Contents/Info.plist"
  File.open(bitbar_path) { |f| Nokogiri::XML(f) }
end

def get_current_version(xml)
  current_version = "0.0.0"
  xml.search("//key").each do |node|
    if (node.content.eql?"CFBundleVersion")
      current_version = node.next_element.content
    end
  end
  current_version
end

begin
  json_val = get_json
  current_version = get_current_version(get_xml)
  latest_version = json_val["tag_name"]
  outdated = Gem::Version.new(current_version) < Gem::Version.new(latest_version.sub!("v", ""))
  color = outdated ? "red" : "green"

  puts current_version + " | color=" + color

  if outdated
    puts "---"
    puts "Download latest (#{latest_version}) | href=" + json_val["assets"][0]["browser_download_url"]
  end
rescue => _
  puts "BitBar Version Error | color=red"
  puts "---"
  puts "Content is currently unavailable. Please try resetting or check your internet connection. | color=red"
end
