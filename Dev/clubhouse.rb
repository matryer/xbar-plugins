#!/usr/bin/env ruby

# <xbar.title>Clubhouse</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Tom Haratyk</xbar.author>
# <xbar.author.github>jazzytomato</xbar.author.github>
# <xbar.desc>Show your Clubhouse.io work and allow to quickly open tickets, associated branches and pull requests</xbar.desc>
# <xbar.dependencies>ruby</xbar.dependencies>
# <xbar.image>https://i.imgur.com/oExbPOg.png</xbar.image>

CLUBHOUSE_API_TOKEN='CHANGE_ME'
CLUBHOUSE_USER='CHANGE_ME'
GITHUB_ORG='CHANGE_ME' # for PRs
CLUBHOUSE_WORKFLOW='Engineering'
CLUBHOUSE_NUMBER_OF_ITEMS=20 # max is 25
CLUBHOUSE_QUERY="owner:#{CLUBHOUSE_USER} !is:done !is:archived" # https://help.clubhouse.io/hc/en-us/articles/360000046646-Search-Operators

BAR_COLORS = true

require 'net/http'
require "uri"
require 'json'
require 'pathname'
require 'openssl'
require 'time'

SCRIPT_PATH = Pathname.new($0).realpath()
REFRESH = "---\nRefresh | refresh=true"

def get_workflows
  uri = URI.parse("https://api.clubhouse.io/api/v3/workflows")
  http = Net::HTTP.new(uri.host, uri.port)
  http.use_ssl = true
  http.verify_mode = OpenSSL::SSL::VERIFY_PEER

  request = Net::HTTP::Get.new(uri.request_uri)
  request["Content-Type"] = "application/json"
  request["Clubhouse-Token"] = CLUBHOUSE_API_TOKEN

  http.request(request)
end

def get_stories
  uri = URI.parse("https://api.clubhouse.io/api/v3/search/stories?page_size=#{CLUBHOUSE_NUMBER_OF_ITEMS}&query=#{CLUBHOUSE_QUERY}")
  http = Net::HTTP.new(uri.host, uri.port)
  http.use_ssl = true
  http.verify_mode = OpenSSL::SSL::VERIFY_PEER

  request = Net::HTTP::Get.new(uri.request_uri)
  request["Content-Type"] = "application/json"
  request["Clubhouse-Token"] = CLUBHOUSE_API_TOKEN

  http.request(request)
end

def format_pr(pr)
  repo = /#{GITHUB_ORG}\/(.*)\/pull/.match(pr['url'])[1]
  icon = pr['closed'] ? ':cl:' : (pr['review_status'] == 'approved' ? ':white_check_mark:' : '')
  updated_at = Time.parse(pr['updated_at'])
  day = updated_at.strftime('%a %d/%m')
  timestamp = (day != Time.now.strftime('%a %d/%m') ? ('on ' + day) : '') + " at " + updated_at.strftime("%I:%M%p")
  "\n----#{icon} #{repo} ##{pr['number']} -> #{pr['target_branch_name']} - Modified on #{timestamp} | href=#{pr['url']}"
end

def format_branch(b)
  "\n----:clipboard:#{b['name']} | terminal=false bash='/bin/bash' param1='-c' param2=\"'echo #{b['name']} | pbcopy'\""
end

def format_labels(item)
  "[" + item['labels'].map { |l| l['name'] }.join(' ') + "] "
end

def create_branch(item)
  name = "ch#{item['id']}/#{item['name'].downcase.gsub(/(?!\s)\W/, '').gsub(/\s+/, '-')[0..50]}"
  ":clipboard::heavy_plus_sign: #{name} | terminal=false bash='/bin/bash' param1='-c' param2=\"'echo #{name} | pbcopy'\""
end

def build_item(item)
  (item['blocked'] ? ':no_entry_sign: ' : '') +
  item['name'].gsub('|', '/') + " " + 
  " | color=black length=80 href=#{item['app_url']}" +
  "\n--#{format_labels(item)}" +
  "\n--:clipboard:Copy story id #{item['id'].to_s} | terminal=false bash='/bin/bash' param1='-c' param2=\"'echo #{item['id']} | pbcopy'\"" +
  "\n--Branches" +
  item['branches'].map(&method(:format_branch)).uniq.join('') +
  (item['branches'].size == 0 ? "\n----" + create_branch(item) : '') +
  "\n--PRs" +
  item['branches'].flat_map { |b| b['pull_requests'].map(&method(:format_pr)) }.join('') +
  "\n"
end

def handle_error(msg, details = "")
  puts """
    :checkered_flag::interrobang:
    ---
    #{msg} | color=red
    #{details}
    ---
    #{REFRESH}
  """
  exit
end

error = nil

response = get_workflows
if response.code != '200'
  handle_error "Error when making request to clubhouse API: HTTP #{response.code}", response.body.to_s
end

workflows = JSON.parse(response.body)
workflows_hash = workflows.find { |w| w['name'] == CLUBHOUSE_WORKFLOW }.fetch('states').reduce({}) { |h, s| h[s['id']] = s['name']; h }

response = get_stories

if response.code != '200'
  handle_error "Error when making request to clubhouse API: HTTP #{response.code}" , response.body.to_s
end

stories = JSON.parse(response.body).fetch('data')

lines = ''

workflows_hash.each do |wfid, wf|
  wf_stories = stories.select { |s| s['workflow_state_id'] == wfid }
  if wf_stories.any?
    lines += wf + "\n"
    lines += wf_stories.map { |s|  build_item(s) }.join("\n")
  end
end

puts """
:checkered_flag:
---
#{lines}
---
#{REFRESH}
"""
