#!/usr/bin/env ruby

# <bitbar.title>Appcenter Status</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Rishabh Tayal</bitbar.author>
# <bitbar.desc>Get status for all your Appcenter apps</bitbar.desc>
# <bitbar.author.github>rishabhtayal</bitbar.author.github>
# <bitbar.dependencies>ruby</bitbar.dependencies>

require 'json'
require 'uri'
require 'net/http'
require 'open-uri'
require 'base64'

# ----------------------------CONFIGURE THE VALUES----------------------------

APPCENTER_API_TOKEN = 'XXXXXXXXXXXXXXXXXXXXXXXX'.freeze

# ----------------------------IGNORE THE LINES BELOW THIS---------------------

# ----------------------------IGNORE THE LINES BELOW THIS----------------------------
class Appcenter
  def self.list_apps
    url = URI('https://api.appcenter.ms/v0.1/apps/')

    http = Net::HTTP.new(url.host, url.port)
    http.use_ssl = true

    request = Net::HTTP::Get.new(url)
    request['X-API-Token'] = APPCENTER_API_TOKEN

    response = http.request(request)
    apps = JSON.parse(response.read_body)
    apps
  end

  def self.branches(owner, app_id)
    url = URI("https://api.appcenter.ms/v0.1/apps/#{owner}/#{app_id}/branches")

    http = Net::HTTP.new(url.host, url.port)
    http.use_ssl = true

    request = Net::HTTP::Get.new(url)
    request['X-API-Token'] = APPCENTER_API_TOKEN

    response = http.request(request)
    app_status = JSON.parse(response.read_body)
    app_status
  end
end

apps = Appcenter.list_apps
puts "Appcenter (#{apps.count})"

puts '---'

apps.each do |app|
  app_name = app['display_name']
  app_id = app['name']
  owner = app['owner']['name']
  image_url = app['icon_url']
  # puts "calling: #{app_id}"
  branches = Appcenter.branches(owner, app_id)
  if branches.is_a?(Array)
    branches = branches.select { |branch| branch['branch']['name'] == 'master' }
  end

  # require 'pry'
  # binding.pry
  color = '#000000'
  if branches.count > 0 && branches.is_a?(Array) && !branches.first['lastBuild'].nil?
    status_string = branches.first['lastBuild']['status']
    color = '#ff0000' if status_string == 'failed'
  else
    color = 'gray'
  end

  if image_url.nil?
    puts "#{app_name} (#{owner}) | href=https://appcenter.ms/orgs/#{owner}/apps/#{app_name}/build/branches color=#{color}"
  else
    # TODO: Fix the image not showing issue
    # image = Base64.encode64(open(image_url, &:read))
    puts "#{app_name} (#{owner}) | href=https://appcenter.ms/orgs/#{owner}/apps/#{app_name}/build/branches color=#{color}"
  end
end
