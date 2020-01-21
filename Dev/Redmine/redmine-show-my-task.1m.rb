#!/usr/bin/env ruby
# coding: utf-8

# <bitbar.title>Redmine Show My Task</bitbar.title>
# <bitbar.version>v1.0.1</bitbar.version>
# <bitbar.author>hikouki</bitbar.author>
# <bitbar.author.github>hikouki</bitbar.author.github>
# <bitbar.desc>Show Redmine open ticket for mine</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/hikouki/bitbar-redmine/master/preview.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/hikouki</bitbar.abouturl>

require 'net/http'
require 'uri'
require 'json'

# a6140cbf6e84a0bAffb0cX49138fc5687310b518
#   or
# launchctl setenv REDMINE_ACCESS_TOKEN a6140cbf6e84a0bAffb0cX49138fc5687310b518
token = ENV["REDMINE_ACCESS_TOKEN"] || ''
# https://redmine.xxxx.com
#   or
# launchctl setenv REDMINE_URL https://redmine.xxxx.com
redmine_url = ENV["REDMINE_URL"] || ''

uri = URI.parse("#{redmine_url}/issues.json?key=#{token}&limit=100&status_id=open&assigned_to_id=me")

begin
  http = Net::HTTP.new(uri.host, uri.port)
  http.use_ssl = true if(uri.scheme == 'https')
  res = http.start {
    http.get(uri.request_uri)
  }

  raise "error #{res.code} #{res.message}" if res.code != '200'

  result = JSON.parse(res.body, symbolize_names: true)
  issues = result[:issues]

  projects = Hash.new do | h, k |
    h[k] = {
      issues_count: 0,
      trackers: Hash.new do | h1, k1 |
        h1[k1] = {
          name: "tracker name.",
          issues: Hash.new {| h2, k2 | h2[k2] = []}
        }
      end
    }
  end

  dark_mode = `defaults read -g AppleInterfaceStyle 2>/dev/null`
  if dark_mode.include? "Dark" then
    main_color = "#FFFFFF"
    project_color = "#FEFEFE"
    issue_color = "#CCCCCC"
    tracker_color = "#33BFDB"
    status_color = "#58BE89"
  else
    main_color = "black"
    project_color = "black"
    issue_color = "black"
    tracker_color = "#33BFDB"
    status_color = "#58BE89"
  end

  last_issue_id = 0

  issues.each do | v |
    last_issue_id = v[:id] if last_issue_id == 0 
    project_id   = v[:project][:id]
    project_name = v[:project][:name]
    status_id    = v[:status][:id]
    tracker_id   = v[:tracker][:id]
    tracker_name = v[:tracker][:name]
    projects[project_id][:issues_count] += 1
    projects[project_id][:id] = project_id
    projects[project_id][:name] = project_name
    projects[project_id][:trackers][tracker_id][:name] = tracker_name
    projects[project_id][:trackers][tracker_id][:issues][status_id].push(v)
  end

  issue_total_count = result[:total_count] > 99 ? '99+' : result[:total_count]
  issue_last = last_issue_id != 0 ? " - ##{last_issue_id}" : ""
  puts issues.empty? ? "✦ | color=#7d7d7d" : "✦ #{issue_total_count}#{issue_last}"
  puts "---"
  puts "Redmine | color=#{main_color} href=#{redmine_url}"
  puts "---"

  projects.each do | _, project |
    puts "#{project[:name]}: #{project[:issues_count]} | color=#{project_color} size=11"
    project[:trackers].each do | _, tracker |
      puts "➠ #{tracker[:name]} | color=#{tracker_color} size=11"
      tracker[:issues].each do | _, status |
        puts "[#{status.first[:status][:name]}] | color=#{status_color} size=11"
        status.each do | issue |
          prefix = status.last == issue ? "└" : "├"
          puts "#{prefix} ##{issue[:id]} #{issue[:subject]} | color=#{issue_color} href=#{redmine_url}/issues/#{issue[:id]} size=11"
        end
      end
    end
    puts "---"
  end
rescue
  puts "✦ ! | color=#ECB935"
  puts "---"
  puts "Exception: #{$!}"
end
