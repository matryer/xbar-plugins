#!/usr/bin/env ruby
# coding: utf-8

# <bitbar.title>TodoColor</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Srdgh</bitbar.author>
# <bitbar.author.github>Srdgh</bitbar.author.github>
# <bitbar.desc>Todo list color-coded</bitbar.desc>
# <bitbar.dependencies>ruby</bitbar.dependencies>

@todo_file = File.open("$HOME/.todo") #todo file path

# Customise here: label color-code (colors optimised for dark theme menubar)
@Labels = { 
    "+Urgent"=>"red",
    ""=>"orange",
    ""=>"yellow",
    ""=>"green",
    ""=>"cadetblue",
    ""=>"purple",
    ""=>"violet"
    }

lines = IO.readlines(@todo_file)
puts "Do: #{lines.length}"

puts "---"

i = 0
until @todo_file.eof() # Until end-of-file
    i += 1
    line = @todo_file.readline().chomp
    if line.include?("+Urgent") #These two lines give +Urgent priority over any other label
        color = "red"
    else    
        @Labels.each do |key, value|
            color = value if line.include?(key)
        end
    end
puts "#{line} | color=#{color}\n"
end
