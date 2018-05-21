#!/usr/bin/env ruby

require 'json'
require 'uri'
require 'net/http'
require 'open-uri'
require 'base64'

# ----------------------------CONFIGURE THE VALUES----------------------------

APPCENTER_API_TOKEN = 'XXXXXXXXXXXXXXXXXXXXXXXX'.freeze

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

  def self.app_status(owner, app_id)
    url = URI("https://api.appcenter.ms/v0.1/apps/#{owner}/#{app_id}/branches/master/builds")

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

  status = Appcenter.app_status(owner, app_id)
  color = '#000000'
  if status.count > 0 && status.is_a?(Array)
    status_string = status.first['result']
    color = '#ff0000' if status_string == 'failed'
  else
    color = 'gray'
  end

  if image_url.nil?
    puts "#{app_name} (#{owner}) | href=https://appcenter.ms/orgs/#{owner}/apps/#{app_name}/build/branches color=#{color}"
  else
    # TODO: Fix the image not showing issue
    image = Base64.encode64(open(image_url, &:read))
    puts "#{app_name} (#{owner}) | href=https://appcenter.ms/orgs/#{owner}/apps/#{app_name}/build/branches color=#{color}"
  end
end
