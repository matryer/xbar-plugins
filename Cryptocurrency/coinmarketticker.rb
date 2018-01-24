#!/usr/bin/env ruby
# encoding: utf-8
#
# <bitbar.title>Coinmarketcap Ticker</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Dennis Theisen</bitbar.author>
# <bitbar.author.github>Soleone</bitbar.author.github>
# <bitbar.desc>
#   Shows selected cryptocurrencies rates from Coinmarketcap using Ruby.
#   * Edit your coins of choice below by editing COINS.
#   * Change your period between 1h, 24h or 7d by editing DEFAULT_PERIOD.
# </bitbar.desc>
# <bitbar.image>https://i.imgur.com/dyJPNy7.png</bitbar.image>

require 'open-uri'
require 'json'
require 'set'

# Edit the coins you care about here:
COINS = %w(
  ETH
  BTC
  XRP
  LTC
  NEO
  MIOTA
).to_set.freeze

DEFAULT_PERIOD = '24h'

URL = 'https://api.coinmarketcap.com/v1/ticker/'

class Coin
  TIME_PERIODS = %w(
    1h
    24h
    7d
  ).freeze

  def initialize(data)
    @data = data
  end

  def price_usd
    format_price(data['price_usd'])
  end

  def name
    data['name']
  end

  def symbol
    data['symbol']
  end

  def rising?(period = DEFAULT_PERIOD)
    sign == '+'
  end

  def falling?(period = DEFAULT_PERIOD)
    sign == '-'
  end

  def to_s
    "#{rising? ? '📈' : '📉'} #{symbol} $#{price_usd} (#{sign}#{percent_change}%)"
  end

  def percent_change(period = DEFAULT_PERIOD)
    format_price(data["percent_change_#{period}"].delete('-'))
  end

  def url
    "https://coinmarketcap.com/currencies/#{name.downcase}"
  end

  private

  attr_reader :data

  def sign(period = DEFAULT_PERIOD)
    percent_change(period).to_f < 0 ? '-' : '+'
  end

  def format_price(amount)
    sprintf('%.2f', amount)
  end
end


def coins
  data = open(URL).read
  all_coin_data = JSON.parse(data)

  all_coin_data.map do |coin_data|
    Coin.new(coin_data)
  end
end

def filtered_coins
  coins.select do |coin|
    COINS.include?(coin.symbol)
  end
end


output = ''

filtered_coins.each do |coin|
  output << "#{coin}\n"
end

puts output
