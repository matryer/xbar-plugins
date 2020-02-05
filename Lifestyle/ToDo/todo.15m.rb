#!/usr/bin/env ruby
# coding: utf-8

# <bitbar.title>Todo Colour</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Srdgh</bitbar.author>
# <bitbar.author.github>Srdgh</bitbar.author.github>
# <bitbar.desc>Todo list with customisable color-code. Mark tasks "done" simply by clicking on them in the menubar drop-down list.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/eNl6QGh.png</bitbar.image>
# <bitbar.dependencies>ruby, todo.sh</bitbar.dependencies>

# Change to your todo directory path (which should contain todo.sh and todo.txt)
todo_directory = "#{Dir.home}/todo/"

# Add further priority labels here
priority_labels = [ "+Urgent" ]

# Change priority color here
priority_color = "red"

# Customise label color-code here (these colors are optimised for a dark theme menubar)
labels = {
  "+Work" => "orange",
  "+Play" => "yellow",
  "+Family" => "green",
  "+Health" => "cadetblue",
  "+Code" => "purple",
  "+Admin" => "violet"
  }

todo_file = File.open("#{todo_directory}todo.txt")
todo_script = ("#{todo_directory}todo.sh")

lines = IO.readlines(todo_file)

puts "Do: #{lines.length}"

puts "---"

line_number = 0
until todo_file.eof() # Until end-of-file
  line_number += 1
  line_color = ""
  line = todo_file.readline().chomp
  priority_labels.each do |priority_label|
    if line.include?(priority_label) # If line contains priority label, display in priority color
      line_color = priority_color
    else # If line contains no priority label, cycle through labels hash, and if line contains a label display in corresponding color
      labels.each { |label, label_color| line_color = label_color if line.include?(label) }
    end
  end
  line_color.empty? ? puts("#{line} | bash=#{todo_script} param1=do param2=#{line_number} terminal=false refresh=\n") : puts("#{line} | color=#{line_color} bash=#{todo_script} param1=do param2=#{line_number} terminal=false refresh=\n") # If the line contains no label, display in default color. Otherwise, in chosen color. Clicking line launches bash script: 'todo.sh do (line_number)'.
end
puts "---"
puts "Click an item to mark 'done'"
puts "Refresh | refresh="
