#!/usr/bin/env ruby
# <xbar.title>Battery Apple Bluetooth keyboard</xbar.title>
# <xbar.version>1.0</xbar.version>
# <xbar.author>Alexandre Espinosa Menor</xbar.author>
# <xbar.author.github>alexandregz</xbar.author.github>
# <xbar.desc>Show battery percentage for Bluetooth Keyboard</xbar.desc>
# <xbar.image>http://i.imgur.com/1FlcPYx.png</xbar.image>
#
# command from https://github.com/matryer/bitbar-plugins/issues/84 by @keithamus
# works fine with Apple Keyboard
#

require 'yaml'

output = YAML.load(`system_profiler SPBluetoothDataType 2> /dev/null`);

output['Bluetooth']['Devices (Paired, Configured, etc.)'].each do |device|
        puts "Keyboard: "+device[1]['Battery Level'].to_s if device[1]['Minor Type'].eql?('Keyboard') && device[1].has_key?('Battery Level')
end
