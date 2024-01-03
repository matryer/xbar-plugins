#!/usr/bin/env ruby
# encoding: UTF-8

# <xbar.title>Notmuch inbox & unread counter</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Slamet Kristanto</xbar.author>
# <xbar.author.github>drselump14</xbar.author.github>
# <xbar.desc>Show mutt inbox using notmuch inbox and unread</xbar.desc>
# <xbar.image>https://www.dropbox.com/s/cgkjb7hv6s1yx1a/Screenshot%202017-02-08%2020.40.02.png?raw=1</xbar.image>
# <xbar.dependencies>mutt,notmuch</xbar.dependencies>

inbox = `/usr/local/bin/notmuch search tag:inbox tag:unread`

mails = inbox.encode('UTF-8', :invalid => :replace).split("\n")
count = mails.length
if count > 0
  puts "📮 #{count} | color=red size=12"
else
  puts ''
end

puts "---"

mails.each do |m|
  mail = m.gsub(/thread.*\]/,'')[0..25].gsub(/\|/,':')
  puts "#{mail} | color=green size=12 bash='/usr/local/bin/mutt' "
end

