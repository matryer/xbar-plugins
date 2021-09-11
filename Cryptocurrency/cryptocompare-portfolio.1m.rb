#!/usr/bin/env ruby

# <bitbar.title>Cryptocompare portfolio holdings</bitbar.title>
# <bitbar.version>v0.1.1</bitbar.version>
# <bitbar.author>Artur Komarov</bitbar.author>
# <bitbar.author.github>mico</bitbar.author.github>
# <bitbar.desc>Shows Cryptocompare portfolio holdings amount</bitbar.desc>
# <bitbar.dependencies>ruby</bitbar.dependencies>

#
# Configuration
# - Create ~/portfolio.yml with 'cookie' value you get from your browser:
# ---
# cookie: __cfduid=...; _ga=...; G_ENABLED_IDPS=google; lightsOff=1; _gid=..; sid=...

require 'open-uri'
require 'net/http'
require 'json'
require 'yaml'

settings = YAML.load(open("#{ENV['HOME']}/portfolio.yml").read)

uri = URI.parse('https://www.cryptocompare.com/portfolio/')
http = Net::HTTP.new(uri.host, uri.port)
http.use_ssl = true
headers = { 'Cookie' => settings['cookie'] }
resp = http.get(uri.request_uri, headers)
page = resp.body
json_data = JSON.parse(page.split("portfolioManager.setPortfolioData(")[1].split(');')[0])
if json_data.include?("Message") && (json_data["Message"].include?("You have to login or register") || json_data["Message"].include?("Something went wrong, please try again later"))
  puts ":exclamation: Please update cookie"
  exit
end

symbols = json_data['Data'][0]['Members'].map { |coin| coin['Coin']['Symbol'] }.uniq.join(',')
prices = JSON.parse(open("https://min-api.cryptocompare.com/data/pricemultifull?fsyms=#{symbols}&tsyms=USD").read)
total = 0
coins = {}
json_data['Data'][0]['Members'].each do |coin|
  symbol = coin['Coin']['Symbol']
  next unless prices['RAW'].include?(symbol)
  if coins.include?(symbol)
    coins[symbol][:amount] += coin['Amount']
  else
    coins[symbol] = {
      amount: coin['Amount'],
      change: prices['RAW'][symbol]['USD']['CHANGEPCT24HOUR']
    }
  end
  coins[symbol][:total] = prices['RAW'][symbol]['USD']['PRICE'] * coins[symbol][:amount]
end
total = coins.reduce(0) { |sum, c| sum + c[1][:total] }
puts total.round(0).to_s
puts '---'
by_change = coins.sort { |a, b| b[1][:change] <=> a[1][:change] }
current = 0
def change_line(symbol, coin, prices)
  puts "#{symbol.ljust(6)} #{prices['RAW'][symbol]['USD']['PRICE'].round(4).to_s.ljust(8)} #{(coin[:change].round(1).to_s+'%').ljust(6)} : #{coin[:total].round(0)}$ | color=#{coin[:change] > 0 ? 'green' : 'red'} href=https://coinmarketcap.com/currencies/#{symbol.downcase}/ alternate=true"
end
coins.sort { |a, b| b[1][:total] <=> a[1][:total] }.each do |symbol, coin|
  puts "#{symbol.ljust(6)} #{prices['RAW'][symbol]['USD']['PRICE'].round(4).to_s.ljust(8)} #{(coin[:change].round(1).to_s+'%').ljust(6)} : #{coin[:total].round(0)}$ | color=#{coin[:change] > 0 ? 'green' : 'red'} href=https://coinmarketcap.com/currencies/#{symbol.downcase}/"
  change_line(by_change[current][0], by_change[current][1], prices)
  current += 1
end

# top value currencies (accumulated positions) - same fields
