#!/usr/bin/env ruby

# <bitbar.title>Heroku apps:errors</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Josh Beckman</bitbar.author>
# <bitbar.author.github>andjosh</bitbar.author.github>
# <bitbar.desc>This plugin displays Heroku errors for a given app. You must be logged in via heroku-cli.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/tAHBNUO.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.dependencies>heroku-toolbelt</bitbar.dependencies>

require 'json'

HEROKU_CMD="/usr/local/bin/heroku"

# Configuration options
app = "appname" # Which app?
hours = 24      # how many hours should the error window contain?


# Heroku Error Glossary
glossary = {
"H10" => "App crashed",
"H11" => "Backlog too deep",
"H12" => "Request timeout",
"H13" => "Connection closed without response",
"H14" => "No web dynos running",
"H15" => "Idle connection",
"H16" => "Redirect to herokuapp.com",
"H17" => "Poorly formatted HTTP response",
"H18" => "Server Request Interrupted",
"H19" => "Backend connection timeout",
"H20" => "App boot timeout",
"H21" => "Backend connection refused",
"H22" => "Connection limit reached",
"H23" => "Endpoint misconfigured",
"H24" => "Forced close",
"H25" => "HTTP Restriction",
"H26" => "Request Error",
"H27" => "Client Request Interrupted",
"H28" => "Client Connection Idle",
"H80" => "Maintenance mode",
"H81" => "Blank app",
"H82" => "Free dyno quota exhausted",
"H99" => "Platform error",
"R10" => "Boot timeout",
"R12" => "Exit timeout",
"R13" => "Attach error",
"R14" => "Memory quota exceeded",
"R15" => "Memory quota vastly exceeded",
"R16" => "Detached",
"R17" => "Checksum error",
"R99" => "Platform error",
"L10" => "Drain buffer overflow",
"L11" => "Tail buffer overflow",
"L12" => "Local buffer overflow",
"L13" => "Local delivery error",
"L14" => "Certificate validation error",
"L15" => "Tail buffer temporarily unavailable"}
total = 0
copy = ""
status = `#{HEROKU_CMD} apps:errors -a #{app} --json --hours #{hours}`
status = JSON.parse(status)

status.keys.each do |level|
    status[level].keys.each do |type|
        if status[level][type].is_a? Numeric
            # router errors
            total += status[level][type]
            copy += "#{type}: #{status[level][type]} (#{glossary[type]})\n"
        else
            status[level][type].keys.each do |error|
                # dyno errors
                total += status[level][type][error]
                copy += "#{error}: #{status[level][type][error]} (#{glossary[error]})\n"
            end
        end
    end
end

puts "#{total} #{app} errors"
puts "---"
puts "In the last #{hours} hours"
puts copy
puts "Refresh... | refresh=true"
