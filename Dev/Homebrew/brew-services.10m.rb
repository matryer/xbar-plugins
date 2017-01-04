#!/usr/bin/env ruby

# <bitbar.title>Brew Services</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Adam Lindberg</bitbar.author>
# <bitbar.author.github>eproxus</bitbar.author.github>
# <bitbar.desc>Shows and manages Homebrew services.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/hVfhHYP.jpg</bitbar.image>
# <bitbar.dependencies>ruby, brew, brew-services</bitbar.dependencies>

# BitBar Homebrew services plugin
# by Adam Lindbeng (@eproxus)

#--- User parameters ----------------------------------------------------------

BAR_COLORS = true

#--- Script internals ---------------------------------------------------------

require 'pathname'

SCRIPT_PATH = Pathname.new($0).realpath()
BREW = "/usr/local/bin/brew"
BREW_LINK = "http://brew.sh/"
BREW_SERVICES = "/usr/local/Homebrew/Library/Taps/homebrew/homebrew-services/cmd/brew-services.rb"
BREW_SERVICES_LINK = "https://github.com/Homebrew/homebrew-services"

REFRESH = "---\nRefresh | refresh=true"

if BAR_COLORS
  DARK_MODE=`defaults read -g AppleInterfaceStyle 2> /dev/null`.strip
  RESET_COLOR = DARK_MODE == 'Dark' ? "\e[37m" : "\e[30m"
else
  RESET_COLOR = "\e[37m"
end

if !File.exist?(BREW)
  puts [
    "Homebrew not installed | color=red",
    "---",
    "Install Homebrew... | href=#{BREW_LINK}",
    REFRESH,
  ].join("\n")
  exit(1)
end
if !File.exist?(BREW_SERVICES)
  puts [
    "Homebrew Services not installed | color=red",
    "---",
    "Install Homebrew Services... | href=#{BREW_SERVICES_LINK}",
    REFRESH,
  ].join("\n")
  exit(1)
end

def green(string)
  "\e[1m\e[32m#{string}#{RESET_COLOR}"
end

def service(command, name)
  "bash=\"#{BREW}\"" \
    + " param1=services param2=#{command} param3=\"#{name}\"" \
    + " terminal=false refresh=true"
end

def menu(name, status, user)
  if status == "started"
    [
      "#{name} | color=#4FFF50",
      "--Restart | #{service("restart", name)}",
      "--Stop | #{service("stop", name)}",
      "-----",
      "--State: #{status}",
      "--User: #{user}",
    ]
  else
    [
      name,
      "--Start | #{service("start", name)}",
      "-----",
      "--State: #{status}",
    ]
  end
end

def plural(count)
  count <= 1 ? "#{count} Service" : "#{count} Services"
end

output = `#{BREW} services list`.split("\n")[1..-1]

services = output && output.reduce({started: 0, menus: []}) do |acc, service|
  name, status, user, _plist = service.split
  acc[:started] += 1 if status == "started"
  acc[:menus] += menu(name, status, user)
  acc
end

total = (output || []).length
started = services[:started]
menus = services[:menus].join("\n")
all = ""
if total > 0
  all = """
All
--Start #{plural(total - started)} | #{service("start", "--all")}
--Stop #{plural(started)} | #{service("stop", "--all")}
--Restart #{plural(total)} | #{service("restart", "--all")}
"""
end

puts """
#{started != 0 && BAR_COLORS ? green(started) : started}/#{total}
---
#{menus}
---
#{all}
#{REFRESH}
"""
