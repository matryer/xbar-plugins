#!/usr/bin/env ruby

# <bitbar.title>Didqus Comments</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Caleb Marble</bitbar.author>
# <bitbar.author.github>marblenix</bitbar.author.github>
# <bitbar.desc>Show comment count per post and number of commenets that are pending.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/marblenix/Disqus-Comments/master/disqus.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/marblenix/Disqus-Comments</bitbar.abouturl>

require 'net/http'
require 'json'

# Get your API key here: https://disqus.com/api/applications/
# You only need "read" permissions and the public key.
public_key = ENV["DISQUS_API_KEY"] || ''
forum = ENV["DISQUS_FORUM"] || 'marblenix' # Disqus shortname

threads_url = "https://disqus.com/api/3.0/forums/listThreads.json" +
           "?api_key=#{public_key}" +
           "&forum=#{forum}"

pending_url = "https://disqus.com/api/3.0/posts/list.json" +
           "?api_key=#{public_key}" +
           "&forum=#{forum}" +
           "&include=unapproved"

thread_uri = URI(threads_url)
pending_uri = URI(pending_url)

thread_results = JSON.parse(Net::HTTP.get(thread_uri))
pending_results = JSON.parse(Net::HTTP.get(pending_uri))

threads = []
pending = []

if thread_results["code"] == 0
    posts = thread_results["response"]
    for post in posts
        next if post["isClosed"] == true
        next if post["isDeleted"] == true
        threads << {
            "link"  => post["link"],
            "title" => post["clean_title"],
            "posts" => post["posts"]
        }
    end
end

if pending_results["code"] == 0
    posts = pending_results["response"]
    for post in posts
        next if post["isSpam"] == true
        next if post["isDeleted"] == true
        next if post["isDeletedByAuthor"] == true
        pending << {
            "createdAt" => post["createdAt"]
        }
    end
end

color = pending.count > 0 ? "red" : "black"

puts "#{forum.capitalize} | color=#{color}"
puts "---"

puts "#{pending.count}\tComment(s) Pending... | href=https://#{forum}.disqus.com/admin/moderate/#/pending"

puts "---"

for thread in threads
    puts "#{thread["posts"]}\t#{thread["title"]} | href=#{thread["link"]}"
end
