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
# Modifications by Guillaume Barrette
#   2017/05/20:
#     - Added Black and White NotePlan menubar icon
#     - Repaired a bug when there was no newline on the last line the done task would get appended to the last line instead of a new line at the end
#     - Added the time in the @done(YYYY-MM-DD HH:MM) so it's like NotePlan preference
#     - Added User Parameters so it's easy to determine if we want to append the @done(...) string at the end of the done task and if we want the black or white menubar icon
#     - Changed the menubar icon to a templateImage so the color changes automatically when using a dark menubar (removed the white icon)
#     - Removed 'use_black_icon' parameters since now it's automatic
#     - Changed encoding method and removed the use of 'force_encoding("utf-8")'
#     - Repaired a bug if there was no file already created for that day in NotePlan
#
# Modifications by Richard Guay
#   05/20/2017:
#       - Added using emoji option
#       - fixed character encoding on removing an item
#       - Proper parsing of [ ] in the todo.
#       - cleanup
require 'date'

#################################
# User Parameters:
insert_date_on_done_task = TRUE
use_emoji = FALSE # If true, will show emoji, otherwise it will use the black or white icon.
use_star = FALSE  # if true, will look for and use '*' instead of '-'
use_icloud = TRUE # If true, files will be checked from iCloud. Otherwise:
use_container = TRUE # If true and not iCloud, it will treat as MAS store version. Otherwise, it is non-MAS store version
#################################

Encoding.default_internal = Encoding::UTF_8
Encoding.default_external = Encoding::UTF_8

todo_file_loc = ""
if use_icloud
    todo_file_loc = File.expand_path("~/Library/Mobile Documents/iCloud~co~noteplan~NotePlan/Documents/Calendar/" + Date.today.strftime('%Y%m%d') + ".txt")
else
    if use_container
        todo_file_loc = File.expand_path("~/Library/Containers/co.noteplan.NotePlan/Data/Library/Application Support/co.noteplan.NotePlan/Calendar/" + Date.today.strftime('%Y%m%d') + ".txt")
    else
        todo_file_loc = File.expand_path("~/Library/Application Support/co.noteplan/Calendar/" + Date.today.strftime('%Y%m%d') + ".txt")
    end
end

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
  linesInFile = []
  if File.exist?("#{todo_file_loc}")
    linesInFile = IO.readlines("#{todo_file_loc}")
  end
  lines = []

  #
  # Remove all lines that are not a todo. Stop at the first empty line.
  #
    linesInFile.each_index { | key |
        #
        # Clean out leading and trailing white spaces (space, tabs, etc)
        #
    line = linesInFile[key].force_encoding("utf-8").gsub(/^\s+/, "").gsub(/\s+$/,"")
    if (line != "") and (! line.include? "[x]")
      #
      # It's a todo line to display. Remove the leading '-' and add
      # to the list.
      #
        if use_star
            lines.push(line.gsub(/^\*\s*(\[ \]\s*)*/,""))
        else
            lines.push(line.gsub(/^\-\s*(\[ \]\s*)*/,""))
        end
    end
  }

  #
  # Give the header. It's an emoji briefcase with the number of items todo
  #
  iconBase64='iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABGdBTUEAALGPC/xhBQAAAAlwSFlzAAAViAAAFYgBxNdAoAAABCRpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOmV4aWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20vZXhpZi8xLjAvIgogICAgICAgICAgICB4bWxuczpkYz0iaHR0cDovL3B1cmwub3JnL2RjL2VsZW1lbnRzLzEuMS8iCiAgICAgICAgICAgIHhtbG5zOnhtcD0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wLyI+CiAgICAgICAgIDx0aWZmOlJlc29sdXRpb25Vbml0PjI8L3RpZmY6UmVzb2x1dGlvblVuaXQ+CiAgICAgICAgIDx0aWZmOkNvbXByZXNzaW9uPjU8L3RpZmY6Q29tcHJlc3Npb24+CiAgICAgICAgIDx0aWZmOlhSZXNvbHV0aW9uPjE0MDwvdGlmZjpYUmVzb2x1dGlvbj4KICAgICAgICAgPHRpZmY6T3JpZW50YXRpb24+MTwvdGlmZjpPcmllbnRhdGlvbj4KICAgICAgICAgPHRpZmY6WVJlc29sdXRpb24+MTQwPC90aWZmOllSZXNvbHV0aW9uPgogICAgICAgICA8ZXhpZjpQaXhlbFhEaW1lbnNpb24+MzI8L2V4aWY6UGl4ZWxYRGltZW5zaW9uPgogICAgICAgICA8ZXhpZjpDb2xvclNwYWNlPjE8L2V4aWY6Q29sb3JTcGFjZT4KICAgICAgICAgPGV4aWY6UGl4ZWxZRGltZW5zaW9uPjMyPC9leGlmOlBpeGVsWURpbWVuc2lvbj4KICAgICAgICAgPGRjOnN1YmplY3Q+CiAgICAgICAgICAgIDxyZGY6QmFnLz4KICAgICAgICAgPC9kYzpzdWJqZWN0PgogICAgICAgICA8eG1wOk1vZGlmeURhdGU+MjAxNzowNToyMCAwMDowNToyMDwveG1wOk1vZGlmeURhdGU+CiAgICAgICAgIDx4bXA6Q3JlYXRvclRvb2w+UGl4ZWxtYXRvciAzLjY8L3htcDpDcmVhdG9yVG9vbD4KICAgICAgPC9yZGY6RGVzY3JpcHRpb24+CiAgIDwvcmRmOlJERj4KPC94OnhtcG1ldGE+CpI9t/8AAAPASURBVFgJtZdLSFRRGMfn/aJRZ5wITQwqCAkzUlrYJrGCLITIateiRbugh7iQCKqVm2pRywgLitBFtchFCVlERQshowckSIllIc44Os44r35nune4c71z5s5gB86c73W+/3e+851zz9gta9wCgUCj2+0+ZrVa51KpVBT3WRmEXaYsRxcMBtsBvgzwDebZnE5nq9frnYrH479lfhwypRmd3+/f43A4+rLZbJfNZnMwJuijSiAf8DEh82OTKUvovKR7AHAB1o1tbjGAjxBIMzI3dEcJH5aKAqihAT4MSB8AAiiHw5hC9pzxqJBBt4VCIb8siEoCcON4kN6lAqsA8K8ymUwdGdigyBooxC2q3mgsOwAW3w94tx4cmfD/BPAeVYfMAb/VCFiVlVWEpL2Zib0qgOqEMYXsNYBiC7Zp9WSkQWO3iiwrAzg+B4BP9QJtQTZG3+1yuQ4hXwVGAO2qvdFoOgBSvwnAI9rVQYvquxQOh8dnZ2eX0un0I0RJHVC1ji9gTQcAeBe9pmD2Pya3+YJErwcX4jfip1gzVQOsvgUHZ7SrVxyCaR2gNk4r/C1Gp0LnBrvd/lXL6+l89HqFyldXV++jkh/Ah1SZwRhXZB6dLpFMJnctLi5+0snzrDQDrHwnK7yPtQxcONMDi+0QBfoecGkGitZAVVVVECeD9PX5cMskCOA2U9KyaUUDYO+uAL7DYN9l/vI65k3wdRzKC4oQhjXA6g/ykXlaKThY4mLq4Xg+LoKbFxu+B7hURG1ElPRXsgUTgF/Ah/QxIqIw3IJoNPolEolc5EHRxk12ALuXwriMtoxtxoy99BTMzMzEcPKM05AgG2NmHGJnIWj1WJacYpgB/ayVlZVv7GlCL5fwUxJdgcpMAE6PxxNgVj89LFaobXpe0Y1rbWR0yQC4CU+y+ocU1TVutXbSeweHSwrwNPxd+B9qINimkb2VgZrW1dfX+9j/SV68vdpJ8NvpJ2prazcKOd+CRuyGkGWhJxF5tfYyWlqEsVhsP9+BzaxKvO/zjc9ujBU3kZXcRTM/P/8d5XGCOMsYpItTYKpJAyCVvwAawVOr1hu35CmCEi/hqxp5loCua3hTpLQGFhYW3uFF3GrTwpvP56sjzcOQ5wnuJqP0nhdzSjVpAEx20VvYhkXxvOZuF+BNBPSCi+peKedm9IVnSjdDfBGVB8VHtmIdwG5MDnMalvnM/tGZV8SWqgHxrLbQ9wL+k7Fzbm5OFNyaNekW8AdTXMEp0Eb5g9EB+Oc1QzbpyMFF1ImtqIX/0v4CwBRdmE9e8GAAAAAASUVORK5CYII='

  if use_emoji
      puts "💼#{lines.length}"
  else
      puts "#{lines.length} |templateImage=#{iconBase64}"
  end

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

  if not linesInFile[-1].include? "\n"
  	linesInFile[-1] = linesInFile[-1] + "\n"
  end

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
      if insert_date_on_done_task
        tm = Time.new
        task = line.chomp + " @done(#{tm.strftime('%Y-%m-%d %H:%M')})\n"
      else
        task = line.chomp + "\n"
      end
      if use_star
        task = task.gsub(/^\*\s*(\[ \]\s*)*/,"* [x] ")
      else
        task = task.gsub(/^\-\s*(\[ \]\s*)*/,"- [x] ")
      end
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
