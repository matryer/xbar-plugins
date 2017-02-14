#!/usr/bin/env ruby
# encoding: UTF-8

# <bitbar.title>Notmuch inbox & unread counter</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Slamet Kristanto</bitbar.author>
# <bitbar.author.github>drselump14</bitbar.author.github>
# <bitbar.desc>Show mutt inbox using notmuch inbox and unread</bitbar.desc>
# <bitbar.image>https://www.dropbox.com/s/cgkjb7hv6s1yx1a/Screenshot%202017-02-08%2020.40.02.png?raw=1</bitbar.image>
# <bitbar.dependencies>mutt,notmuch</bitbar.dependencies>

inbox = `/usr/local/bin/notmuch search tag:inbox tag:unread`

mails = inbox.split("\n")
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

