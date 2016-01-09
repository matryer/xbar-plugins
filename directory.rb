#!/usr/bin/env ruby

# Simple ruby script to output the directory for use in the readme
# https://github.com/dkhamsing

VERSION = '0.1.0'

def dir
  exclude = [
    'README.md',
    'Enabled',
    'directory.rb',
    'directory.md'
  ]

  d = Dir.glob '**/*'
  exclude.each { |x| d.delete x }
  d
end

def output_file(filename)
  if filename.include? '.py'
    filetype = 'python'
  elsif filename.include? '.rb'
    filetype = 'ruby'
  elsif filename.include? '.sh'
    filetype = 'shell'
  elsif filename.include? '.js'
    filetype = 'javascript'
  else
    filetype = 'other'
  end

  extensions = [
    '.js',
    '.py',
    '.rb',
    '.js',
    '.sh'
  ]

  dontrename = [
    'cmus',
    'cpu',
    'elb',
    'GeoIPWeather',
    'itunes',
    'is bitbar',
    'openui5ver',
    'real cpu usage chart',
    'real cpu usage',
    'usbInfo',
    'vpn check',
    'wifiname'
  ]

  filtered = filename
  extensions.each { |e| filtered = filtered.sub e, '' }

  filtered = filtered.gsub /^(.*?)\//, ''
  filtered = filtered.gsub /^(.*?)\//, '' # this could be improved

  filtered = filtered.gsub /\..*/, ''
  filtered = filtered.gsub /-|_/, ' '

  filtered = filtered.split.map(&:capitalize).join(' ') unless dontrename.include? filtered

  md = "[#{filtered}](#{filename})"

  "#{md} `#{filetype}`"
end

def output_directory(x)
  if x.include? '/'
    filtered = x.gsub /^(.*?)\//, ''
    "\n##### #{filtered}"
  else
    "\n#### #{x}"
  end
end

# start
temp = ''
count = 0
dir.each do |x|
  if File.file? x
    temp << "- #{output_file x} \n"
    count += 1
  else
    temp << "#{output_directory(x)} \n"
  end
end

output = "### Available Plugins \n"
output << "#{count} plugins \n"
output << temp

puts "Writing to directory.md"
File.write 'directory.md', output
