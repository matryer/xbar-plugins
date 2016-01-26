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

@urgentColor = "red"

# Customise here: label color-code (colors optimised for dark theme menubar)
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

i = 0
until @todo_file.eof() # Until end-of-file
    i += 1
    color = nil
    line = @todo_file.readline().chomp
    if line.include?("+Urgent") #This line gives +Urgent priority over any other label
        puts "#{line} | color=#{@urgentColor}\n" 
    else
        @labels.each do |key, value|
            color = value if line.include?(key)
        end
        color.nil? ? puts("#{line}\n") : puts("#{line} | color=#{color}\n")
    end
end
