#!/usr/bin/env ruby
# <xbar.title>Battery Apple Bluetooth Trackpad</xbar.title>
# <xbar.version>1.0</xbar.version>
# <xbar.author>Maxime Bertheau</xbar.author>
# <xbar.author.github>maxoumime</xbar.author.github>
# <xbar.desc>Show battery percentage for Bluetooth Trackpad</xbar.desc>
# <xbar.image>http://i.imgur.com/1FlcPYx.png</xbar.image>
#
# A lot of thanks to @alexandregz, I just changed "Keyboard" to "Trackpad" :grin:
# Works fine with Apple Trackpad
#

require 'yaml'

output = YAML.load(`system_profiler SPBluetoothDataType`);

output['Bluetooth']['Devices (Paired, Configured, etc.)'].each do |device|
        puts "Trackpad: "+device[1]['Battery Level'].to_s if device[1]['Minor Type'].eql?('Trackpad') && device[1].has_key?('Battery Level')
end
