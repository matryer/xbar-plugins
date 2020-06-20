#!/usr/bin/ruby
# coding: utf-8

# <bitbar.title>SolarEdge Power Monitoring</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Tristan Harmer</bitbar.author>
# <bitbar.author.github>gondalez</bitbar.author.github>
# <bitbar.desc>Displays current power stats and today's energy stats. Includes energy meter data like import, export and self consumption. Note power data is available only when you have a meter installed. E.g.: https://www.solaredge.com/aus/products/metering-and-sensors/solaredge-modbus-meter. Values are only updated by SolarEdge at most every 15 minutes.</bitbar.desc>
# <bitbar.image>https://cln.sh/3hAQwbgqXms1CtJsi71C</bitbar.image>
# <bitbar.dependencies>solaredge,ruby</bitbar.dependencies>

require 'net/http'
require 'json'

API_KEY = '<your key>'
SITE_ID = '<your site id>'

TIME_FORMAT = '%Y-%m-%d %H:%M:%S'

Details = Struct.new(
  :purchased,
  :feedin,
  :selfconsumption,
  :production,
  :consumption,
  keyword_init: true,
) do
  def importing?
    purchased > feedin
  end

  def grid_flow
    if importing?
      purchased
    else
      feedin
    end
  end
end

# type: power|energy
def get_details(type)
  end_time = Time.now
  start_time = end_time - 1

  params = {
    startTime: start_time.strftime(TIME_FORMAT),
    endTime: end_time.strftime(TIME_FORMAT),
    api_key: API_KEY,
  }

  query = URI.encode_www_form(params)

  url = "https://monitoringapi.solaredge.com/site/#{SITE_ID}/#{type}Details.json?#{query}"

  json = Net::HTTP.get(URI(url))

  values =
    JSON
      .parse(json)
      .dig("#{type}Details", 'meters')
      .map{ |meter| [meter['type'].downcase.to_sym, meter['values'].first.dig('value')] }
      .to_h

  Details.new(**values)
end

def format_kw(value)
  return '-' if value.nil?
  formatted = (value/1000).round(1)
  "#{formatted}kW"
end

def format_kwh(value)
  "#{format_kw(value)}h"
end

# data
power = nil
energy = nil

threads = [
  Thread.new { power = get_details('power') },
  Thread.new { energy = get_details('energy') },
]

threads.each(&:join)

# render
font_option = "font='sf compact text regular' size=12"
link_option = "href=https://monitoring.solaredge.com/solaredge-web/p/site/#{SITE_ID}/"

flow_arrow = if power.importing?
               '↢'
             else
               '↣'
             end

puts("☀️#{format_kw(power.production)} ↣ 🏡#{format_kw(power.consumption)} #{flow_arrow} 🏭#{format_kw(power.grid_flow)}|#{font_option}")
puts '---'
puts 'Energy Today'
puts("Produced: #{format_kwh(energy.production)}|#{font_option} #{link_option}")
puts("Consumed: #{format_kwh(energy.consumption)}|#{font_option} #{link_option}")
puts("Self Consumed: #{format_kwh(energy.selfconsumption)}|#{font_option} #{link_option}")
puts("Imported: #{format_kwh(energy.purchased)}|#{font_option} #{link_option}")
puts("Exported: #{format_kwh(energy.feedin)}|#{font_option} #{link_option}")
