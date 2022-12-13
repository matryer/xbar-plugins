#!/usr/bin/env ruby
# <xbar.title>Coinbase.com Account Balances</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.dependencies>ruby</xbar.dependencies>
# <xbar.author>Bryan Stone</xbar.author>
# <xbar.author.github>aegixx</xbar.author.github>
# <xbar.desc>Shows your balances in BTC & USD.  Be sure you add you API key & secret.</xbar.desc>

require 'coinbase/wallet'

API_KEY = 'YOUR_API_KEY'
API_SECRET = 'YOUR_SECRET_KEY'
LAST_BTC_FILE = '~/.bitbar_last_btc_rate'

def getDelta(current, last)
  if current != last
    delta = current - last
    if delta.positive?
      { color: 'green', symbol: '▲', amount: delta.abs }
    else
      { color: 'red', symbol: '▼', amount: delta.abs }
    end
  end
end

def printPrimary
  usd_total = @client.accounts.collect {|a| a.native_balance.amount}.inject(:+)
  btc_total = @client.accounts.collect {|a| a.balance.amount}.inject(:+)
  if @delta
    printf "1Ƀ = %.2f (#{@delta[:symbol]}%.2f) | color=#{@delta[:color]}\n", @current_btc_rate, @delta[:amount]
    printf "∑ $%.2f (#{@delta[:symbol]}%.2f) | color=#{@delta[:color]}\n", usd_total, (@delta[:amount] * btc_total)
  else
    printf "1Ƀ = %.2f\n", @current_btc_rate
    printf "∑ $%.2f\n", usd_total
  end
end

def printBalance(account)
  if @delta
    printf "%s: $%.2f (#{@delta[:symbol]}%.2f) | color=#{@delta[:color]}\n", account.name, account.native_balance.amount, (@delta[:amount] * account.balance.amount)
  else
    printf "%s: $%.2f\n", account.name, account.native_balance.amount
  end
end

@client = Coinbase::Wallet::Client.new(api_key: API_KEY, api_secret: API_SECRET)

last_path = File.expand_path(LAST_BTC_FILE)
@last_btc_rate = BigDecimal.new(File.read(last_path)) if File.exist?(last_path)
@current_btc_rate = @client.spot_price.amount
@delta = getDelta(@current_btc_rate, @last_btc_rate)
File.open(last_path, 'w') {|f| f.write(@current_btc_rate) }

printPrimary
puts '---'
@client.accounts.each do |account|
  printBalance(account)
end
