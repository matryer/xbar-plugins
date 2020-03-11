#!/usr/bin/env ruby
# <bitbar.title>Battery Apple Bluetooth keyboard</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>Alexandre Espinosa Menor</bitbar.author>
# <bitbar.author.github>alexandregz</bitbar.author.github>
# <bitbar.desc>Show battery percentage for Bluetooth Keyboard</bitbar.desc>
# <bitbar.image>http://i.imgur.com/1FlcPYx.png</bitbar.image>
#
# command from https://github.com/matryer/bitbar-plugins/issues/84 by @keithamus
# works fine with Apple Keyboard
#

require 'yaml'

output = YAML.load(`system_profiler SPBluetoothDataType 2> /dev/null`);

output['Bluetooth']['Devices (Paired, Configured, etc.)'].each do |device|
        puts "Keyboard: "+device[1]['Battery Level'].to_s if device[1]['Minor Type'].eql?('Keyboard') && device[1].has_key?('Battery Level')
end
