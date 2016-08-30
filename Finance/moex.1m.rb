#!/usr/bin/env ruby

# <bitbar.title>The Russian Ruble exchange rates</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Roman Krasavtsev</bitbar.author>
# <bitbar.author.github>romankrasavtsev</bitbar.author.github>
# <bitbar.author.github.url>https://github.com/RomanKrasavtsev/moex-bitbar-plugin</bitbar.author.github.url>
# <bitbar.desc>The Russian Ruble exchange rates from Moscow Exchange</bitbar.desc>
# <bitbar.dependencies>ruby</bitbar.dependencies>

require "nokogiri"
require "open-uri"

def get_exchange_rate emoji, *currencies
  result_string = ""

  currencies.each do |currency|
    pair, sign = get_pair_sign emoji, currency

    if pair
      rate = Nokogiri::HTML(open("http://moex.com/ru/derivatives/currency-rate.aspx?currency=#{pair}"))
        .css("#ctl00_PageContent_tbxCurrentRate b")
        .to_s.gsub(/<b>/, "").gsub(/<\/b>/, "")
        .gsub(/Текущее значение:  /, "")
        .slice /\d+,\d./
    else
      rate = "Unknown sign"
    end

    result_string += "#{sign} #{rate}  "
  end

  result_string
end

def get_pair_sign emoji, currency
  case currency
  when "USD"
    pair, sign  = ["USD_RUB", emoji ? "🇺🇸" : "$"]
  when "EUR"
    pair, sign = ["EUR_RUB", emoji ? "🇪🇺" : "€"]
  when "CHF"
    pair, sign = ["CHF_RUB", emoji ? "🇨🇭" : "Fr"]
  when "JPY"
    pair, sign = ["JPY_RUB", emoji ? "🇯🇵" : "J¥"]
  when "CNY"
    pair, sign = ["CNY_RUB", emoji ? "🇨🇳" : "C¥"]
  when "CAD"
    pair, sign = ["CAD_RUB", emoji ? "🇨🇦" : "C$"]
  when "TRY"
    pair, sign  = ["TRY_RUB", emoji ? "🇹🇷" : "₺"]
  else
    pair, sign = [nil, ""]
  end
end

# Supported currencies:
# USD - United States dollar
# EUR - Euro
# CHF - Swiss franc
# JPY - Japanese yen
# CNY - Chinese yuan
# CAD - Canadian dollar
# TRY - Turkish lira

puts get_exchange_rate emoji = false, "USD", "EUR", "CAD"
