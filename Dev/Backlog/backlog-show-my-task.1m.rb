#!/usr/bin/env ruby
# coding: utf-8

# <bitbar.title>Backlog Show My Task</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>hikouki</bitbar.author>
# <bitbar.author.github>hikouki</bitbar.author.github>
# <bitbar.desc>Show Backlog open ticket for mine</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/hikouki/bitbar-backlog/master/preview.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/hikouki</bitbar.abouturl>

require 'net/http'
require 'uri'
require 'json'

# a6140cbf6e84a0bAffb0cX49138fc5687310b518
#   or
# launchctl setenv BACKLOG_ACCESS_TOKEN a6140cbf6e84a0bAffb0cX49138fc5687310b518
TOKEN = ENV["BACKLOG_ACCESS_TOKEN"] || ''
# https://xxxx.backlog.jp
#   or
# launchctl setenv BACKLOG_URL https://xxxx.backlog.jp
BACKLOG_URL = ENV["BACKLOG_URL"] || ''

begin

  myself_api = URI.parse("#{BACKLOG_URL}/api/v2/users/myself")
  myself_api.query = URI.encode_www_form(apiKey: TOKEN)

  myself_res = Net::HTTP.start(myself_api.host, myself_api.port, use_ssl: myself_api.scheme == 'https') do | http |
    http.get(myself_api.request_uri)
  end

  raise "error #{myself_res.code} #{myself_res.message}" if myself_res.code != '200'

  me = JSON.parse(myself_res.body, symbolize_names: true)

  issues_api = URI.parse("#{BACKLOG_URL}/api/v2/issues")
  issues_api.query = URI.encode_www_form(apiKey: TOKEN, 'assigneeId[]' => me[:id], count: 100)

  issues_res = Net::HTTP.start(issues_api.host, issues_api.port, use_ssl: issues_api.scheme == 'https') do | http |
    http.get(issues_api.request_uri)
  end

  raise "error #{issues_res.code} #{issues_res.message}" if issues_res.code != '200'

  issues = JSON.parse(issues_res.body, symbolize_names: true)

  projects = Hash.new do | h, k |
    h[k] = {
      name: "<project name>",
      issue_count: 0,
      issue_type_groups: Hash.new do | h1, k1 |
        h1[k1] = {
          name: "<type name>",
          color: "<type color>",
          issue_status_groups: Hash.new do | h2, k2 |
            h2[k2] = {
              name: "<status name>",
              issues: []
            }
          end
        }
      end
    }
  end

  issues.each do | issue |
    project_id = issue[:projectId]
    issue_type_id = issue[:issueType][:id]
    issue_status_id = issue[:status][:id]

    projects[project_id][:issue_count] += 1
    projects[project_id][:issue_type_groups][issue_type_id][:name] = issue[:issueType][:name]
    projects[project_id][:issue_type_groups][issue_type_id][:color] = issue[:issueType][:color]
    projects[project_id][:issue_type_groups][issue_type_id][:issue_status_groups][issue_status_id][:name] = issue[:status][:name]
    projects[project_id][:issue_type_groups][issue_type_id][:issue_status_groups][issue_status_id][:issues].push(
      {
        key: issue[:issueKey],
        summary: issue[:summary]
      }
    )
  end

  puts issues.empty? ? "◈ | color=#7d7d7d" : "◈ #{issues.count}"
  puts "---"
  puts "Backlog | color=black href=#{BACKLOG_URL}"
  puts "---"

  projects.each do | project_id, project |
    project_api = URI.parse("#{BACKLOG_URL}/api/v2/projects/#{project_id}")
    project_api.query = URI.encode_www_form(apiKey: TOKEN)

    project_res = Net::HTTP.start(project_api.host, project_api.port, use_ssl: project_api.scheme == 'https') do | http |
      http.get(project_api.request_uri)
    end

    raise "error #{project_res.code} #{project_res.message}" if project_res.code != '200'

    project_info = JSON.parse(project_res.body, symbolize_names: true)

    puts "#{project_info[:name]}: #{project[:issue_count]} | size=11"
    project[:issue_type_groups].each do | _, type_group |
      puts "➠ #{type_group[:name]} | color=#{type_group[:color]} size=11"
      type_group[:issue_status_groups].each do | _, status_group |
        puts "[#{status_group[:name]}] | color=#58BE89 size=11"
        status_group[:issues].each do | issue |
          prefix = status_group[:issues].last == issue ? "└" : "├"
          puts "#{prefix} #{issue[:summary]} | color=black href=#{BACKLOG_URL}/view/#{issue[:key]} size=11"
        end
      end
    end
    puts "---"
  end

  puts "---"
  puts "Refresh | color=#7d7d7d refresh=true"

rescue => e
  puts "◈ ! | color=#ECB935"
  puts "---"
  puts "Exception: #{$!}"
  puts e.backtrace
end
