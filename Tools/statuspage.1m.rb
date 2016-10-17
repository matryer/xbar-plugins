#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

# <bitbar.title>StatusPage.io</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>Stephen Yeargin</bitbar.author>
# <bitbar.author.github>stephenyeargin</bitbar.author.github>
# <bitbar.desc>Show a StatusPage.io's Status in BitBar</bitbar.desc>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.image>http://i.imgur.com/FsD4zDD.png</bitbar.image>

require 'open-uri'
require 'json'

# BEGIN Configuration #

statuspage_id = 'YOUR_ID_HERE' # e.g. 4y7j9y37gcns
url = "https://#{statuspage_id}.statuspage.io/api/v2/summary.json"

# END Configuration #

status_map = {
  operational: {
    name: 'Operational',
    color: 'gold'
  },
  degraded_performance: {
    name: 'Degraded Performance',
    color: 'orange'
  },
  partial_outage: {
    name: 'Partial Outage',
    color: 'yellow'
  },
  major_outage: {
    name: 'Major Outage',
    color: 'red'
  }
}

begin
  raise 'Missing configuration.' if statuspage_id == 'YOUR_ID_HERE'

  summary = JSON.parse(open(url).read)

  puts summary['status']['description']
  puts '---'
  puts "#{summary['page']['name']}|href=#{summary['page']['url']}"
  puts summary['status']['description']
  puts '---'
  summary['components'].each do |component|
    next unless component['status'] != 'operational'
    puts "#{status_map[component['status'].to_sym][:name]}: "\
      "#{component['name']}"\
      "|color=#{status_map[component['status'].to_sym][:color]}"
  end
  puts "Open: #{summary['page']['url']}|href=#{summary['page']['url']}"
rescue StandardError => e
  puts 'Unable to load status!|color=red'
  puts e.message
  exit 1
end
