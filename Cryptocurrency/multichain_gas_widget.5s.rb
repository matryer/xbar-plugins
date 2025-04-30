#!/usr/bin/env ruby

# <bitbar.title>Ethereum Gas Fees</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Masumi Kawasaki</bitbar.author>
# <bitbar.author.github>geeknees</bitbar.author.github>
# <bitbar.desc>Widget for monitoring Gas Fees from https://etherscan.io/</bitbar.desc>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.image>https://raw.githubusercontent.com/geeknees/xbar-plugins/main/eth_gas_widget/screenshot.png</bitbar.image>
# <bitbar.abouturl>https://github.com/geeknees/xbar-plugins</bitbar.abouturl>

require 'open-uri'
require 'json'

CHAINID = 1 # Ethereum mainnet. See other available chains at https://api.etherscan.io/v2/chainlist and endpoints at https://forms.blockscan.com/public/grid/3E9QiN00NLhCQVibiP3Z-Bpqhmd7zGXsgapEKJupxiI
ENDPOINT = "ENDPOINT = https://api.etherscan.io/v2/api?chainid=#{CHAINID}&module=gastracker&action=gasoracle&apikey="
APIKEY = "" # ETHERSCAN API KEY

charset = nil
html = URI.open(ENDPOINT+APIKEY) do |f|
  charset = f.charset
  f.read
end

response = JSON.parse(html)

puts "⟠ #{response['result']['ProposeGasPrice'].to_f.floor(6)}"
puts '---'

response['result'].each do |k, v|
  puts "#{k}: #{v.to_f.floor(6)}"
end
