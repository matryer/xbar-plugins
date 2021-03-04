#!/usr/bin/env ruby
# <bitbar.title>Wikipedia On This Day</bitbar.title>
# <bitbar.version>0.1.0</bitbar.version>
# <bitbar.author>Ryan Scott Lewis</bitbar.author>
# <bitbar.author.github>RyanScottLewis</bitbar.author.github>
# <bitbar.desc>Display Wikipedia On This Day information.</bitbar.desc>
# <bitbar.dependencies>ruby (wikipedia, wikicloth, nokogiri rubygems)</bitbar.dependencies>

require "wikipedia" # gem install wikipedia-client
require "wikicloth" # gem install wikicloth
require "nokogiri"  # gem install nokogiri

def convert_wiki_to_html(data)
  WikiCloth::Parser.new(data: data).to_html
end

day_str = Time.now.strftime("%B %e")
page = Wikipedia.find(day_str, prop: "revisions", rvprop: "content")

within_events_section = false
lines = page.content.lines.each_with_object([]) do |line, memo|

  html = convert_wiki_to_html(line)
  html_document = Nokogiri::HTML(html)

  headline_node = html_document.xpath("//h2/span[@class='mw-headline']").first

  if !headline_node.nil?
    within_events_section = headline_node.text == "Events"
  else
    next unless within_events_section

    list_item_node = html_document.xpath("//ul/li")

    memo <<  list_item_node.text
  end
end

puts "ⓦ"
puts "---"
lines.each { |line| puts(line) }

# TODO:
# * Links!
# * Sort by date
# * Date format
# * Strip references: "[1]", etc
