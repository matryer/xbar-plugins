#!/usr/bin/env ruby
# coding: utf-8

# <bitbar.title>TodoColour</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Srdgh</bitbar.author>
# <bitbar.author.github>Srdgh</bitbar.author.github>
# <bitbar.desc>Todo list color-coded</bitbar.desc>
# <bitbar.image>http://i.imgur.com/eNl6QGh.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>

@todo_file = File.open("#{Dir.home}/.todo") #todo file path

# Add further priority labels here
@priorityLabels = [ "+Urgent" ]

# Change priority color here
@priorityColor = "red"

# Customise label color-code here (these colors are optimised for a dark theme menubar)
@labels = { 
    "+Work"=>"orange",
    "+Play"=>"yellow",
    "+Family"=>"green",
    "+Health"=>"cadetblue",
    "+Code"=>"purple",
    "+Admin"=>"violet"
    }

lines = IO.readlines(@todo_file)
puts "Do: #{lines.length}"

puts "---"

until @todo_file.eof() # Until end-of-file
    color = nil
    line = @todo_file.readline().chomp
    @priorityLabels.each do |key| # If line contains priority label, display in priority color
        color = @priorityColor if line.include?(key)
    end
    if color.nil? # If line contains no priority label, check for other label and if present display in chosen color
        @labels.each do |key, value|
        color = value if line.include?(key)
        end
    end    
    color.nil? ? puts("#{line}\n") : puts("#{line} | color=#{color}\n") # If the line contains no label, display in default color
end
