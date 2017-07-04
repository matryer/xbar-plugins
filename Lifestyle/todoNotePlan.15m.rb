#!/usr/bin/env ruby
# coding: utf-8

# <bitbar.title>NotePlan Todo in Colour</bitbar.title>
# <bitbar.version>v1.5</bitbar.version>
# <bitbar.author>Richard Guay</bitbar.author>
# <bitbar.author.github>raguay</bitbar.author.github>
# <bitbar.desc>A todo list taken from NotePlan and displayed with customizable color-code. Mark tasks "done" simply by clicking on them in the menubar drop-down list. This was based on "Todo Colour" plugin by Srdgh.</bitbar.desc>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.image>http://customct.com/images/NotePlanPlugin-01.png</bitbar.image>
# <bitbar.abouturl>http://customct.com/bitbar</bitbar.abouturl>
#
# Modifications by Guillaume Barrette
#   2017/07/01:
#     - Added option to show subtasks
#   2017/06/15:
#     - Changed TRUE/FALSE constant to true/false since uppercase are deprecated in ruby 2.4
#     - Changed labels to start with '#' to follow NotePlan way of tagging
#     - Allow to change Fonts by the user
#     - Added a new parameter for users to specify if want the task to be archived at the end of the file or not
#     - Added alternate action to mark as cancelled instead of done (using the Option modifier key)
#     - Allow indentation at beginning of task
#   2017/06/03:
#     - Added 'divide_with_header' to allow to show sections separated by headers
#     - Updated the algorithm to skip all items that are not a task (Skip anything that doesn't starts with '- ' or '* ' and if followed by [x], [>], [-])
#   2017/05/28:
#     - Fixed the line number of item to mark as done by getting the id before stripping the lines that are not a task
#     - Scheduled task (to another day - [>]) are now skipped also
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
insert_date_on_done_task = true  # If true, the date would be inserted with the @done tag
use_emoji = false                # If true, will show emoji, otherwise it will use the black or white icon.
use_star = false                 # if true, will look for and use '*' instead of '-'
show_alt_task = true             # If true, tasks marked with the alternate character ('* ' if use_star is FALSE or '- ' if use_star is TRUE) would be shown in the task list. For example, this could be useful to use them as bullet list.
show_subtasks = true             # If true, subtasks would be shown in the list
divide_with_header = true        # If true, headers would be listed and a separator is put between lists
archive_task_at_end = false      # If true, the task would get archived to the end of the note
use_icloud = true                # If true, files will be checked from iCloud. Otherwise:
use_container = true             # If true and not iCloud, it will treat as MAS store version. Otherwise, it is non-MAS store version

standard_font = ''               # Font used for tasks
header_font   = 'Helvetica-Bold' # Font used for headers if listed with 'divide_with_header'
#################################

Encoding.default_internal = Encoding::UTF_8
Encoding.default_external = Encoding::UTF_8

todo_file_loc = ''
if use_icloud
  todo_file_loc = File.expand_path('~/Library/Mobile Documents/iCloud~co~noteplan~NotePlan/Documents/Calendar/' + Date.today.strftime('%Y%m%d') + '.txt')
else
  if use_container
    todo_file_loc = File.expand_path('~/Library/Containers/co.noteplan.NotePlan/Data/Library/Application Support/co.noteplan.NotePlan/Calendar/' + Date.today.strftime('%Y%m%d') + '.txt')
  else
    todo_file_loc = File.expand_path('~/Library/Application Support/co.noteplan/Calendar/' + Date.today.strftime('%Y%m%d') + '.txt')
  end
end

if ARGV.empty?
  #
  # Add further priority labels here
  #
  priority_labels = ['@urgent', '@due']

  #
  # Change priority color here
  #
  priority_color = 'red'

  #
  # Customise label color-code here:
  #
  labels = {
    '@Work' => 'orange',
    '@Play' => 'yellow',
    '@home' => 'green',
    '@daily' => 'blue',
    '@Health' => 'cadetblue',
    '@church' => 'lightblue',
    '@tutorials' => 'violet',
    '@Envato' => 'darkorange',
    '@workflow' => 'purple',
    '@tutorial' => 'cobaltblue'
  }

  linesInFile = File.exist?(todo_file_loc.to_s) ? IO.readlines(todo_file_loc.to_s) : []
  lines = []

  #
  # Remove all lines that are not a todo. Stop at the first empty line.
  #
  line_number = []
  line_number_id = 0
  taskStyleToSearch = (show_alt_task) ? ['- ', '* '] : (use_star) ? ['* '] : ['- ']
  linesInFile.each_index do |key|
    #
    # Clean out leading and trailing white spaces (space, tabs, etc)
    #
    line = linesInFile[key].gsub(/\s+$/, '')
    taskLine = show_subtasks ? line.gsub(/^\s+/, '') : line
    if (taskLine.start_with?(*taskStyleToSearch)) && (!taskLine[2..4].start_with?('[x]', '[>]', '[-]'))  # Get only active Task items
      #
      # It's a todo line to display. Remove the leading '-' and add
      # to the list.
      #
      if use_star
        lines.push(line.gsub(/^(\s*)\*\s*(\[ \]\s*)*/, '\1'))
      else
        lines.push(line.gsub(/^(\s*)\-\s*(\[ \]\s*)*/, '\1'))
      end
      line_number.push(line_number_id)
    elsif divide_with_header && line.start_with?('#')
      lines.push(line)
      line_number.push(line_number_id)
    end
    line_number_id += 1
  end

  #
  # Give the header. It's the NotePlan icon or an emoji briefcase with the number of items todo
  #
  iconBase64 = 'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABGdBTUEAALGPC/xhBQAAAAlwSFlzAAAViAAAFYgBxNdAoAAABCRpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOmV4aWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20vZXhpZi8xLjAvIgogICAgICAgICAgICB4bWxuczpkYz0iaHR0cDovL3B1cmwub3JnL2RjL2VsZW1lbnRzLzEuMS8iCiAgICAgICAgICAgIHhtbG5zOnhtcD0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wLyI+CiAgICAgICAgIDx0aWZmOlJlc29sdXRpb25Vbml0PjI8L3RpZmY6UmVzb2x1dGlvblVuaXQ+CiAgICAgICAgIDx0aWZmOkNvbXByZXNzaW9uPjU8L3RpZmY6Q29tcHJlc3Npb24+CiAgICAgICAgIDx0aWZmOlhSZXNvbHV0aW9uPjE0MDwvdGlmZjpYUmVzb2x1dGlvbj4KICAgICAgICAgPHRpZmY6T3JpZW50YXRpb24+MTwvdGlmZjpPcmllbnRhdGlvbj4KICAgICAgICAgPHRpZmY6WVJlc29sdXRpb24+MTQwPC90aWZmOllSZXNvbHV0aW9uPgogICAgICAgICA8ZXhpZjpQaXhlbFhEaW1lbnNpb24+MzI8L2V4aWY6UGl4ZWxYRGltZW5zaW9uPgogICAgICAgICA8ZXhpZjpDb2xvclNwYWNlPjE8L2V4aWY6Q29sb3JTcGFjZT4KICAgICAgICAgPGV4aWY6UGl4ZWxZRGltZW5zaW9uPjMyPC9leGlmOlBpeGVsWURpbWVuc2lvbj4KICAgICAgICAgPGRjOnN1YmplY3Q+CiAgICAgICAgICAgIDxyZGY6QmFnLz4KICAgICAgICAgPC9kYzpzdWJqZWN0PgogICAgICAgICA8eG1wOk1vZGlmeURhdGU+MjAxNzowNToyMCAwMDowNToyMDwveG1wOk1vZGlmeURhdGU+CiAgICAgICAgIDx4bXA6Q3JlYXRvclRvb2w+UGl4ZWxtYXRvciAzLjY8L3htcDpDcmVhdG9yVG9vbD4KICAgICAgPC9yZGY6RGVzY3JpcHRpb24+CiAgIDwvcmRmOlJERj4KPC94OnhtcG1ldGE+CpI9t/8AAAPASURBVFgJtZdLSFRRGMfn/aJRZ5wITQwqCAkzUlrYJrGCLITIateiRbugh7iQCKqVm2pRywgLitBFtchFCVlERQshowckSIllIc44Os44r35nune4c71z5s5gB86c73W+/3e+851zz9gta9wCgUCj2+0+ZrVa51KpVBT3WRmEXaYsRxcMBtsBvgzwDebZnE5nq9frnYrH479lfhwypRmd3+/f43A4+rLZbJfNZnMwJuijSiAf8DEh82OTKUvovKR7AHAB1o1tbjGAjxBIMzI3dEcJH5aKAqihAT4MSB8AAiiHw5hC9pzxqJBBt4VCIb8siEoCcON4kN6lAqsA8K8ymUwdGdigyBooxC2q3mgsOwAW3w94tx4cmfD/BPAeVYfMAb/VCFiVlVWEpL2Zib0qgOqEMYXsNYBiC7Zp9WSkQWO3iiwrAzg+B4BP9QJtQTZG3+1yuQ4hXwVGAO2qvdFoOgBSvwnAI9rVQYvquxQOh8dnZ2eX0un0I0RJHVC1ji9gTQcAeBe9pmD2Pya3+YJErwcX4jfip1gzVQOsvgUHZ7SrVxyCaR2gNk4r/C1Gp0LnBrvd/lXL6+l89HqFyldXV++jkh/Ah1SZwRhXZB6dLpFMJnctLi5+0snzrDQDrHwnK7yPtQxcONMDi+0QBfoecGkGitZAVVVVECeD9PX5cMskCOA2U9KyaUUDYO+uAL7DYN9l/vI65k3wdRzKC4oQhjXA6g/ykXlaKThY4mLq4Xg+LoKbFxu+B7hURG1ElPRXsgUTgF/Ah/QxIqIw3IJoNPolEolc5EHRxk12ALuXwriMtoxtxoy99BTMzMzEcPKM05AgG2NmHGJnIWj1WJacYpgB/ayVlZVv7GlCL5fwUxJdgcpMAE6PxxNgVj89LFaobXpe0Y1rbWR0yQC4CU+y+ocU1TVutXbSeweHSwrwNPxd+B9qINimkb2VgZrW1dfX+9j/SV68vdpJ8NvpJ2prazcKOd+CRuyGkGWhJxF5tfYyWlqEsVhsP9+BzaxKvO/zjc9ujBU3kZXcRTM/P/8d5XGCOMsYpItTYKpJAyCVvwAawVOr1hu35CmCEi/hqxp5loCua3hTpLQGFhYW3uFF3GrTwpvP56sjzcOQ5wnuJqP0nhdzSjVpAEx20VvYhkXxvOZuF+BNBPSCi+peKedm9IVnSjdDfBGVB8VHtmIdwG5MDnMalvnM/tGZV8SWqgHxrLbQ9wL+k7Fzbm5OFNyaNekW8AdTXMEp0Eb5g9EB+Oc1QzbpyMFF1ImtqIX/0v4CwBRdmE9e8GAAAAAASUVORK5CYII='

  lineCount = 0
  lines.each { |line|  lineCount += 1 unless line.start_with?('#') }
  if use_emoji
    puts "💼#{lineCount}"
  else
    puts "#{lineCount} |templateImage=#{iconBase64}"
  end

  puts '---'

  cfn = File.expand_path(__FILE__)

  #
  # Create the list of items to do in the menu.
  #
  item_number = 0
  lines.each do |item|
    line_color = ""
    line = item.chomp
    if priority_labels.any? { |s| line.include? s }
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
    #
    # If the line contains no label, display in default color. Otherwise, in
    # chosen color. Clicking line launches this script with line number as
    # the parameter.
    #
    line_font = standard_font
    if line.start_with?('#')
      puts('---') unless line.start_with?('##')
      line_font = header_font
    end
    lineParams = "#{line_color.empty? ? '' : 'color='+line_color} #{line_font.empty? ? '' : 'font='+line_font} bash='#{cfn}' param1=#{line_number[item_number]}"
    puts("#{line} | " + lineParams + " param2=x terminal=false trim=false refresh=\n")
    puts("#{line} | alternate=true " + lineParams + " param2=- terminal=false trim=false refresh=\n")
    item_number += 1
  end
  puts '---'
  puts "Click an item to mark as 'done'"
  puts "Click an item to mark as 'cancelled' | alternate=true"
  puts 'Refresh | refresh='
else
  #
  # This is what to do when clicking on an item. We want to move
  # the item to the Archive section and set it as done. If there
  # isn't an Archive area, create it and add the task to it.
  #
  # Get the task number to archive.
  #
  doNum = ARGV[0].to_i
  mark = ARGV[1]

  #
  # Get the list of todos and setup variables
  #
  todo_file = File.open(todo_file_loc.to_s)
  linesInFile = IO.readlines(todo_file)

  if !linesInFile[doNum].start_with?('#')  # Do nothing if the item is a header
    task = ''
    lines = []
    line_number = 0

    linesInFile[-1] = linesInFile[-1] + "\n" unless linesInFile[-1].include? "\n"

    #
    # Process the todo list lines.
    #
    linesInFile.each do |line|
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
          task = line.chomp + (mark == 'x' ? " @done(#{Time.new.strftime('%Y-%m-%d %H:%M')})\n" : "\n")
        else
          task = line.chomp + "\n"
        end
        task = task.gsub(/^(\s*)([\-\*]+)\s*(\[ \]\s*)*/, '\1\2 [' + mark + '] ')  # Works with both task style, useful if mix with 'show_alt_task', also it keeps the indentation at beginning of the line
        lines.push(task) unless archive_task_at_end
      end
      line_number += 1
    end

    #
    # Add the task to the bottom.
    #
    lines.push(task) if archive_task_at_end

    #
    # Save the file.
    #
    IO.write(todo_file, lines.join)
  end
end
