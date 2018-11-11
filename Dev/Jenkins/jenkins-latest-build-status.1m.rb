#!/usr/bin/env ruby

# Jenkins Build Status
# by Tony Mai (thetonymai@gmail.com)

# <bitbar.title>Jenkins Build Status</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Tony Mai</bitbar.author>
# <bitbar.author.github>tonymai</bitbar.author.github>
# <bitbar.desc>Shows the latest builds of a Jenkins project. Result, Build ID, Timestamp, Duration.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/tonymai/jenkins-bitbar-plugin/master/screenshot.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/tonymai/jenkins-bitbar-plugin</bitbar.abouturl>

require 'net/http'
require 'json'

# Variables To Fill Out

USERNAME = 'username'.freeze
AUTH_TOKEN = 'auth_token'.freeze
URL = 'https://url/'.freeze # must have trailing slash '/'
NAME = 'Jenkins'.freeze

# Information Requests

def get(url)
  uri = URI(url)
  Net::HTTP.start(uri.host, uri.port, use_ssl: uri.scheme == 'https') do |http|
    request = Net::HTTP::Get.new(uri)
    request.basic_auth(USERNAME, AUTH_TOKEN)
    response = http.request(request)
    JSON.parse(response.body)
  end
end

def latest_builds(limit = 5)
  json = get(URL + 'api/json')
  return unless json.key?('builds')

  json['builds'].take(limit).map { |build| get(build['url'] + 'api/json') }
end

# Pretty Display Formatters

def format_status(status)
  case status
  when 'SUCCESS' then "\u{2714}"
  when 'FAILURE' then "\u{2718}"
  else "\u{2022}"
  end
end

def format_color(status)
  case status
  when 'SUCCESS' then 'green'
  when 'FAILURE' then 'red'
  else 'yellow'
  end
end

def format_timestamp(timestamp)
  Time.at(timestamp / 1000).strftime('%b %e %I:%M%P')
end

def format_duration(time_in_ms)
  time_in_sec = time_in_ms / 1000
  minutes = time_in_sec / 60
  seconds = time_in_sec % 60
  "#{minutes}m #{seconds}s"
end

# Print Helpers

def print_build_details(build)
  puts "Last Build (##{build['id']})"
  build['actions'].each do |action|
    next unless action['causes']

    action['causes'].each do |cause|
      puts cause['shortDescription'] if cause['shortDescription']
    end
  end
end

def print_builds_summary(builds)
  puts 'Latest Builds'
  builds.each do |build|
    id = build['id']
    status = format_status(build['result'])
    time = format_timestamp(build['timestamp'])
    duration = format_duration(build['duration'])
    url = build['url']
    color = format_color(build['result'])
    puts "#{status} ##{id}: #{time} (#{duration}) | href=#{url} color=#{color}"
  end
end

# Driver Code

def run
  builds = latest_builds
  return puts 'No builds executing' unless builds

  last = builds.first
  puts format_status(last['result']) + ' ' + NAME
  puts '---'
  print_build_details(last)
  puts '---'
  print_builds_summary(builds)
  puts '---'
  puts 'Open In Browser | href= ' + URL
end

run
