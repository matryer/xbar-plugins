#!/usr/bin/ruby
# coding: utf-8
#
# <bitbar.title>Digitalocean</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Alejandro Torres</bitbar.author>
# <bitbar.desc>List your Digitalocean droplets.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/GV9FXrE.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/fulgorek</bitbar.abouturl>

require 'json'
require 'net/https'

module DigitalOcean
  class Client
    DIGITALOCEAN_API = 'https://api.digitalocean.com'

    attr_reader :access_token

    def initialize(options = {})
      @access_token = options[:access_token] || no_access_token
      @get = options[:get] || 'no_method'
      self.send(@get) if defined? @get
      render_info
    end


    def droplets
      show_toolbar_icon
      fetch("#{DIGITALOCEAN_API}/v2/droplets")
      data = JSON.parse(@response.body)
      no_droplets_handle if data['droplets'].empty?
      # process_droplets(data)
      fake
      create_droplet
    end

    private

    def render_info
      puts '---'
      puts 'Web console | href=https://cloud.digitalocean.com'
    end

    def create_droplet
      puts '---'
      puts 'Create Droplet | href=https://cloud.digitalocean.com/droplets/new?size=512mb'
    end

    def show_toolbar_icon
      puts '|image=iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAQAAAC1QeVaAAAA4klEQVQYGQXBMSuEAQAG4Oe77066H0CJ0tmkSAaFQbFjUUaT36AMSpKSbNhkNhmslCx3ZTAohYiUsjql83qeAqUOFiwaUvPh0qkvpQ5V9LkSERHxaQkl9HgREU/utUXECnAhomVaXZeGIxE/GsyKuFUFwK6IAw5EzKFboVBD3bu4r+jHqxb+lPhV+tZEgz4jBpUACiXOxG8BYMyEHxe+UPNg0DMDNkxjT8QkWBdxzLzYxo6IUWyK+DNc6DFuxpRrN+qWzegFa44osC+2wKOIb6uoVFV0nLjTRJc3becOvSh1/gGbP1qNI14GZAAAAABJRU5ErkJggg=='
      puts '---'
    end

    def process_droplets(data)
      data['droplets'].map do |d|
        name = d['name']
        region = d['region']['slug']
        ipv4 = d['networks']['v4'][0]['ip_address']
        color = d['status'] == 'active' ? '#222222' : '#FF0000'
        text = "[#{ipv4}] #{name}"
        spaces = (17 - text.split(' ').first.length + 2)
        puts text.gsub(' ', ' ' * spaces) + " |color=#{color}"
      end
    end

    def open_config_file
      `/usr/bin/open -t #{File.dirname(__FILE__)}`
    end

    def no_access_token
      show_toolbar_icon
      puts 'No access token! | color=red'
      puts "Open config file | bash=\"#{open_config_file}\" terminal=false refresh=false\n"
      exit
    end

    def no_method
      puts 'Please set method'
    end

    def handle_errors
      puts 'Error!'
      exit
    end

    def no_droplets_handle
      puts 'No Droplets!'
      create_droplet
      render_info
      exit
    end

    def fetch(uri)
      uri = URI.parse(uri)
      https = Net::HTTP.new(uri.host, uri.port)
      https.use_ssl = true
      req = Net::HTTP::Get.new(uri.path, initheader = connection_options)
      @response = https.request(req)
      handle_errors if @response.code != '200'
    end

    def connection_options
      {
        'Content-Type' => 'application/json',
        'Authorization' => "Bearer #{access_token}"
      }
    end
  end
end

DigitalOcean::Client.new({
  :access_token => '',
  :get => 'droplets'
})
