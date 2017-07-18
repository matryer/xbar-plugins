#!/usr/bin/env ruby
# <bitbar.title>Battery Apple Bluetooth Trackpad</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>Maxime Bertheau</bitbar.author>
# <bitbar.author.github>maxoumime</bitbar.author.github>
# <bitbar.desc>Show battery percentage for Bluetooth Trackpad</bitbar.desc>
# <bitbar.image>http://i.imgur.com/1FlcPYx.png</bitbar.image>
#
# A lot of thanks to @alexandregz, I just changed "Keyboard" to "Trackpad" :grin:
# Works fine with Apple Trackpad
#

require 'yaml'

output = YAML.load(`system_profiler SPBluetoothDataType`);

output['Bluetooth']['Devices (Paired, Configured, etc.)'].each do |device|
        puts "Trackpad: "+device[1]['Battery Level'].to_s if device[1]['Minor Type'].eql?('Trackpad') && device[1].has_key?('Battery Level')
end
