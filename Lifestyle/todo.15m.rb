#!/usr/bin/env ruby
# coding: utf-8

# <bitbar.title>TodoColor</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Srdgh</bitbar.author>
# <bitbar.author.github>Srdgh</bitbar.author.github>
# <bitbar.desc>Todo list with customisable color-code. Mark tasks "done" simply by clicking on them in the menubar drop-down list.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/eNl6QGh.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>

# Change to your todo directory path
todo_directory = "/Users/alexanderbarker/Code/CLI/todo.txt_cli-2.9/"

# Add further priority labels here
priority_labels = [ "+Urgent" ]

# Change priority color here
priority_color = "red"

# Customise label color-code here (these colors are optimised for a dark theme menubar)
labels = {
  "+Write" => "orange",
  "+Wedding" => "yellow",
  "+Shopping" => "lightgreen",
  "+Reports" => "cadetblue",
  "+Admin" => "purple",
  "+DoS" => "violet",
  "+Mark" => "darkgreen"
  }

todo_file = File.open("#{todo_directory}todo.txt")
todo_script = ("#{todo_directory}todo.sh")

lines = IO.readlines(todo_file)
puts "#{lines.length} | color=lightgreen"

puts "---"

linenumber = 0
until todo_file.eof() # Until end-of-file
  linenumber += 1
  line_color = ""
  line = todo_file.readline().chomp
  priority_labels.each do |priority_label|
    if line.include?(priority_label) # If line contains priority label, display in priority color
      line_color = priority_color 
    else # If line contains no priority label, cycle through labels hash, and if line contains a label display in corresponding color
      labels.each { |label, label_color| line_color = label_color if line.include?(label) }
    end
  end
  line_color.empty? ? puts("#{line} | bash=#{todo_script} param1=do param2=#{linenumber} terminal=false refresh=\n") : puts("#{line} | color=#{line_color} bash=#{todo_script} param1=do param2=#{linenumber} terminal=false refresh=\n") # If the line contains no label, display in default color. Otherwise, in chosen color. Clicking line launches bash script: 'todo.sh do (linenumber)'.
end
puts "---"
puts "Click an item to mark 'done'"
puts "Refresh | refresh="