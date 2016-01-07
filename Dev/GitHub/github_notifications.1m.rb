#!/usr/bin/env ruby

# Simple ruby script to check for GitHub notifications
# https://github.com/dkhamsing/bitbar_github_notifications

require 'open-uri'
require 'json'

VERSION = '0.1.0'

GITHUB_USERS = ['']

GITHUB_ACCESS_TOKENS = ['']

GITHUB_REPO_API = 'https://api.github.com/notifications'

CONFIG_NOTIFICATIONS_YES = '☗'
CONFIG_NOTIFICATIONS_NO = '☖'

def get_notifications
  n = {}
  GITHUB_USERS.each_with_index do |u, i|
    token = GITHUB_ACCESS_TOKENS[i]
    n[u] = get_user_notifications u, token
  end
  n
end

def get_user_notifications(login, token)

  c = open GITHUB_REPO_API, http_basic_authentication: [login, token]

  JSON.parse c.read
end

def process(notifications)
  notifications.each do |name, value|
    if value.count == 0
      puts "No notifications for #{name}"
    else
      puts name
      value.each do |x|
            r = x['repository']
            s = x['subject']
            url = s['url']
            href = process_link_api_to_html url
            puts "#{r['full_name']}: #{s['title']} | href=#{href}"
      end
    end
    separator
  end
end

def process_link_api_to_html(url)
  url.sub('api.', '').sub('repos/', '')
    .sub('pulls', 'pull')
    .sub('commits', 'commit')
end

def separator
  puts '---'
end

def title(notifications)
  count = 0
  notifications.each { |_, value| count+=value.count}
  t = count == 0 ? CONFIG_NOTIFICATIONS_NO : CONFIG_NOTIFICATIONS_YES
  notifications.each do |name, value|
    t << " #{name}: #{value.count} " if value.count > 0
  end
  puts t
end

begin
n = get_notifications
rescue => e
  puts 'GitHub Notifications | color=red'
  puts "Error: #{e}"
  exit
end
title n
separator
process n
