#!/usr/bin/env ruby

# <xbar.title>Foobot</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Alessio Signorini</xbar.author>
# <xbar.author.github>alessio-signorini</xbar.author.github>
# <xbar.desc>Display readings from your sensors in the menu bar</xbar.desc>
# <xbar.image>https://user-images.githubusercontent.com/453354/93032602-57f2c580-f5e7-11ea-87f9-e4e559b0c12a.png</xbar.image>
# <xbar.dependencies>ruby</xbar.dependencies>
# <xbar.abouturl>https://foobot.io/</xbar.abouturl>



# ==============================================================================
# SETTINGS
# ==============================================================================

# Request API key from
#    http://api.foobot.io/apidoc/index.html
# <xbar.var>string(API_KEY=""): API Key</xbar.var>

# Get Device ID from
#    http://api.foobot.io/apidoc/index.html#!/device-owner-controller/getDeviceUsingGET
# <xbar.var>string(DEVICE_ID=""): Device ID</xbar.var>

# ==============================================================================



















# ==============================================================================
# CODE
# ==============================================================================
require 'net/http'
require 'json'

DEVICE_ID = ENV['DEVICE_ID'] || abort("DEVICE_ID variable not defined")
API_KEY   = ENV['API_KEY'] || abort("API_KEY variable not defined")

uri = URI("http://api.foobot.io/v2/device/#{DEVICE_ID}/datapoint/0/last/600/")

req = Net::HTTP::Get.new(uri)
req['Accept'] = 'application/json;charset=UTF-8'
req['X-API-KEY-TOKEN'] = API_KEY

res = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(req)
}

data = JSON.parse(res.body)

def color(value, ok_threshold, bad_threshold)
  return ' color=red' if value > bad_threshold
  return ' color=orange' if value > ok_threshold
  return ''
end

value = data['sensors'].each_with_index.map do |key, i|
  [key, data['datapoints'].first[i] ]
end.to_h

puts "🌿 #{value['allpollu'].round} | size=12" + color(value['allpollu'],50,75)

puts "---"

puts "#{(value['tmp'] * 9 / 5).round + 32}F / #{value['hum'].round}%"
puts "pm #{value['pm'].round}ugm3 | " + color(value['pm'],27,37.5)
puts "co2 #{value['co2'].round}ppm | " + color(value['co2'], 1300,1925)
puts "voc #{value['voc'].round}ppb | " + color(value['voc'], 300, 450)
