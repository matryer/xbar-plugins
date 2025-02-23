#!/usr/bin/env ruby

# -*- coding: utf-8 -*-

# frozen_string_literal: true

# <xbar.title>StatusPage.io</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Stephen Yeargin</xbar.author>
# <xbar.author.github>stephenyeargin</xbar.author.github>
# <xbar.desc>Show a StatusPage.io's Status in BitBar</xbar.desc>
# <xbar.dependencies>ruby</xbar.dependencies>
# <xbar.image>https://raw.githubusercontent.com/matryer/xbar-plugins/refs/heads/main/Tools/statuspage.1m.png</xbar.image>

require 'json'
require 'net/http'
require 'uri'

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
  # raise if statuspage_id is missing or is set to the default
  raise 'Missing StatusPage.io ID' if statuspage_id == 'YOUR_ID_HERE' || statuspage_id.empty?

  uri = URI(url)
  response = Net::HTTP.get(uri)
  summary = JSON.parse(response)

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
