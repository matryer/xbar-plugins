#!/usr/bin/env ruby
# coding: utf-8

# <bitbar.title>NotePlan Todo in Colour</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Richard Guay</bitbar.author>
# <bitbar.author.github>raguay</bitbar.author.github>
# <bitbar.desc>A todo list taken from NotePlan and displayed with customizable color-code. Mark tasks "done" simply by clicking on them in the menubar drop-down list. This was based on "Todo Colour" plugin by Srdgh.</bitbar.desc>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.image>http://customct.com/images/NotePlanPlugin-01.png</bitbar.image>
# <bitbar.abouturl>http://customct.com/bitbar</bitbar.abouturl>
#
require 'date'

todo_file_loc = File.expand_path("~/Library/Mobile Documents/iCloud~co~noteplan~NotePlan/Documents/Calendar/" + Date.today.strftime('%Y%m%d') + ".txt")

if ARGV.empty?
  #
  # Add further priority labels here
  #
  priority_labels = [ "@Urgent", "@due" ]

  #
  # Change priority color here
  #
  priority_color = "red"

  #
  # Customise label color-code here:
  #
  labels = {
    "@Work" => "orange",
    "@Play" => "yellow",
    "@home" => "green",
    "@daily" => "blue",
    "@Health" => "cadetblue",
    "@church" => "lightblue",
    "@tutorials" => "violet",
    "@Envato" => "darkorange",
    "@workflow" => "purple",
    "@tutorial" => "cobaltblue"
  }

  linesInFile = IO.readlines("#{todo_file_loc}")
  lines = []

  #
  # Remove all lines that are not a todo. Stop at the first empty line.
  #
  linesInFile.each_index { | key |
    #
    # Clean out leading and trailing white spaces (space, tabs, etc)
    #
    line = linesInFile[key].gsub(/^\s+/, "").gsub(/\s+$/,"")
    if (line != "") and (! line.include? "[x]")
      #
      # It's a todo line to display. Remove the leading '-' and add
      # to the list.
      #
      lines.push(line.gsub(/^\-/,""))
    end
  }

  #
  # Give the header. It's an emoji briefcase with the number of items todo
  #
  puts "💼#{lines.length}"

  puts "---"

  cfn = File.expand_path(__FILE__)

  #
  # Create the list of items to do in the menu.
  #
  line_number = 0
  lines.each { |item|
    line_number += 1
    line_color = ""
    line = item.chomp
    priority_labels.each do |priority_label|
      if line.include?(priority_label)
        #
        # If line contains priority label, display in priority color
        #
        line_color = priority_color
      else
        #
        # If line contains no priority label, cycle through labels hash,
        # and if line contains a label display in corresponding color
        #
        labels.each { |label, label_color| line_color = label_color if line.include?(label) }
      end
    end
    #
    # If the line contains no label, display in default color. Otherwise, in
    # chosen color. Clicking line launches this script with line number as
    # the parameter.
    #
    line_color.empty? ? puts("#{line} | bash='#{cfn}' param1=#{line_number} terminal=false refresh=\n") : puts("#{line} | color=#{line_color} bash='#{cfn}' param1=#{line_number} terminal=false refresh=\n")
  }
  puts "---"
  puts "Click an item to mark 'done'"
  puts "Refresh | refresh="
else
  #
  # This is what to do when clicking on an item. We want to move
  # the item to the Archive section and set it as done. If there
  # isn't an Archive area, create it and add the task to it.
  #
  # Get the task number to archive.
  #
  doNum = ARGV[0].to_i

  #
  # Get the list of todos and setup variables
  #
  todo_file = File.open("#{todo_file_loc}")
  linesInFile = IO.readlines(todo_file)
  task = ""
  lines = []
  line_number = 0

  #
  # Process the todo list lines.
  #
  linesInFile.each { | line |
    line_number += 1
    if line_number != doNum
      #
      # It is one of the other lines. Just push it into the stack.
      #
      lines.push(line)
    else
      #
      # Get the line to be moved to the archive area.
      #
      tm = Time.new
      task = line.chomp + " @done(#{tm.year}-#{tm.month}-#{tm.day})\n"
      task = task.gsub(/^\-/,"- [x]")
    end
  }

  #
  # Add the task to the bottom.
  #
  lines.push(task)

  #
  # Save the file.
  #
  IO.write(todo_file,lines.join)
end
