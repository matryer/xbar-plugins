#!/usr/bin/env ruby
# coding: utf-8

# <bitbar.title>Todo Today for NotePlan v3</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Jonathan Clark</bitbar.author>
# <bitbar.author.github>jgclark</bitbar.author.github>
# <bitbar.desc>A todo list taken from NotePlan v3 and displayed with customizable color-code. Mark tasks "done" simply by clicking on them in the menubar drop-down list. This was based on "Todo.NotePlan" by Richard Guay which in turn was based on "Todo Colour" plugin by Srdgh.</bitbar.desc>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.image></bitbar.image>
# <bitbar.abouturl>https://noteplan.co/</bitbar.abouturl>
#
# Modifications by Jonathan Clark
#   2020/10/30:
#     - Update NP data storage filepaths for NotePlan 3 beta
#       (including CloudKit change at v3.0.15 beta)
#     - Make CloudKit location the default
#     - tweak colours and falgs to suit my needs
#     - ignore tasks with dates scheduled into the future
#     - improve some non-tasks it was including
#     - code clean up
#   2020/11/29:
#     - auto-detect storage type (CloudKit > iCloud Drive > Drobpox if there are multiple)
#     - add option to specify the file extension in use (default to md, but can be txt)
#
# Modifications by Guillaume Barrette
#   2017/07/01:
#     - Added option to show subtasks
#   2017/06/15:
#     - Changed TRUE/FALSE constant to true/false since uppercase are deprecated in ruby 2.4
#     - Changed labels to start with '#' to follow NotePlan way of tagging
#     - Allow to change Fonts by the user
#     - Added a new parameter for users to specify if want the task to be archived
#       at the end of the file or not
#     - Added alternate action to mark as cancelled instead of done (using the
#       Option modifier key)
#     - Allow indentation at beginning of task
#   2017/06/03:
#     - Added 'divide_with_header' to allow to show sections separated by headers
#     - Updated the algorithm to skip all items that are not a task (Skip anything that
#       doesn't starts with '- ' or '* ' and if followed by [x], [>], [-])
#   2017/05/28:
#     - Fixed the line number of item to mark as done by getting the id before stripping
#       the lines that are not a task
#     - Scheduled task (to another day - [>]) are now skipped also
#   2017/05/20:
#     - Added Black and White NotePlan menubar icon
#     - Repaired a bug when there was no newline on the last line the done task would
#       get appended to the last line instead of a new line at the end
#     - Added the time in the @done(YYYY-MM-DD HH:MM) so it's like NotePlan preference
#     - Added User Parameters so it's easy to determine if we want to append the
#       @done(...) string at the end of the done task and if we want the black or white
#       menubar icon
#     - Changed the menubar icon to a templateImage so the color changes automatically
#       when using a dark menubar (removed the white icon)
#     - Removed 'use_black_icon' parameters since now it's automatic
#     - Changed encoding method and removed the use of 'force_encoding("utf-8")'
#     - Repaired a bug if there was no file already created for that day in NotePlan
#
# Modifications by Richard Guay
#   2017/05/20:
#       - Added using emoji option
#       - fixed character encoding on removing an item
#       - Proper parsing of [ ] in the todo.
#       - cleanup
require 'date'

#################################
# User Parameters:
insert_date_on_done_task = true  # If true, the date would be inserted with the @done tag
use_emoji_as_icon = false        # If true, will show emoji, otherwise it will use the black or white icon.
use_star = true                  # if true, will look for and use '*' instead of '-'
show_alt_task = true             # If true, tasks marked with the alternate character ('* ' if use_star is FALSE or '- ' if use_star is TRUE) would be shown in the task list. For example, this could be useful to use them as bullet list.
show_subtasks = true             # If true, subtasks would be shown in the list
divide_with_header = true        # If true, headers would be listed and a separator is put between lists
archive_task_at_end = false      # If true, the task would get archived to the end of the note
file_extension = '.md'            # Defaults to file extension type 'md' -- can change to '.txt'

standard_font = ''               # Font used for tasks
header_font   = 'Helvetica-Bold' # Font used for headers if listed with 'divide_with_header'
#################################

Encoding.default_internal = Encoding::UTF_8
Encoding.default_external = Encoding::UTF_8

USERNAME = ENV['LOGNAME'] # pull username from environment
USER_DIR = ENV['HOME'] # pull home directory from environment
DROPBOX_DIR = "#{USER_DIR}/Dropbox/Apps/NotePlan/Documents".freeze
ICLOUDDRIVE_DIR = "#{USER_DIR}/Library/Mobile Documents/iCloud~co~noteplan~NotePlan/Documents".freeze
CLOUDKIT_DIR = "#{USER_DIR}/Library/Containers/co.noteplan.NotePlan3/Data/Library/Application Support/co.noteplan.NotePlan3".freeze
data_root_filepath = DROPBOX_DIR if Dir.exist?(DROPBOX_DIR) && Dir[File.join(DROPBOX_DIR, '**', '*')].count { |file| File.file?(file) } > 1
data_root_filepath = ICLOUDDRIVE_DIR if Dir.exist?(ICLOUDDRIVE_DIR) && Dir[File.join(ICLOUDDRIVE_DIR, '**', '*')].count { |file| File.file?(file) } > 1
data_root_filepath = CLOUDKIT_DIR if Dir.exist?(CLOUDKIT_DIR) && Dir[File.join(CLOUDKIT_DIR, '**', '*')].count { |file| File.file?(file) } > 1

todo_file_loc = File.expand_path(data_root_filepath + '/Calendar/' + Date.today.strftime('%Y%m%d') + file_extension)

if ARGV.empty?
  # Add further priority labels here
  priority_labels = ['@urgent', '#high']

  # Change priority color here
  priority_marker = '🔴'

  # Customise label color-code here:
  labels = {
    '@admin' => 'orange',
    '@liz' => 'yellow',
    '@home' => 'green',
    '@martha' => 'purple', # pink is too light
    '@Health' => 'cadetblue',
    '@church' => 'blue', # lightblue is too light
    '@tutorials' => 'violet',
    '@Envato' => 'darkorange',
    '@workflow' => 'purple',
    '@tutorial' => 'cobaltblue'
  }

  lines_in_file = File.exist?(todo_file_loc.to_s) ? IO.readlines(todo_file_loc.to_s) : []
  lines = []

  # Remove all lines that are not a todo. Stop at the first empty line.
  line_number = []
  line_number_id = 0
  task_style_to_search = show_alt_task ? ['- ', '* '] : use_star ? ['* '] : ['- ']
  lines_in_file.each_index do |key|
    # Clean out leading and trailing white spaces (space, tabs, etc)
    line = lines_in_file[key].gsub(/\s+$/, '')
    task_line = show_subtasks ? line.gsub(/^\s+/, '') : line
    if task_line.start_with?(*task_style_to_search) && !task_line[2..4].start_with?('[x]', '[>]', '[-]')  # Get only active Task items
      # Now check if doesn't have a >YYYY-MM-DD that schedules it into the future
      break if task_line =~ /\s>\d{4}\-\d{2}\-\d{2}/

      # It's a todo line to display. Remove the leading task marker and add to the list.
      if use_star
        lines.push(line.gsub(/^(\s*)\*\s*(\[ \]\s*)*/, '\1'))
      else
        lines.push(line.gsub(/^(\s*)\-\s*(\[ \]\s*)*/, '\1'))
      end
      line_number.push(line_number_id)
    elsif divide_with_header && line =~ /^#+\s+/ # i.e. this is a header line
      lines.push(line)
      line_number.push(line_number_id)
    end
    line_number_id += 1
  end

  # Give the header. It's the NotePlan icon or an emoji briefcase with the number of items todo
  icon_base64 = 'iVBORw0KGgoAAAANSUhEUgAAADgAAAA4CAQAAAACj/OVAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAAmJLR0QA/4ePzL8AAAAJcEhZcwAAFiUAABYlAUlSJPAAAAAHdElNRQfkChwAHRNqrC5wAAABSElEQVRYw2NgGAWjYBRQCXAy8AIxnYAxw384NKKHhf9R4KiFoxYOpIXGDPxkWsjFYEKqZQ5Q476QYSFMRpfcYJtDgoU3yQvuZ2iG/mfwIsLCBgxdl8hLFhD4h0EMj4VKWPX8p8RCEHzPwILVwv84IZGgHY8RG9H4t/GodSU+FmPwGEMsVCI1axygwLIF5Gb+V2RY9oGy8oaV4R9J1rFQo5BLIdIyR2qWrBcJWLaL2nXELwIWnqCmZROJDNIsalgmT1KS+cMgTIllzCSmUAh8A0zZZIEXFGT8Y6Ra1kCFoq2IeOt24DEG3Skb8Khtprx6wlYfMjJ8pbR6wq8Zm6gwZRZextCoRUQTwx1D113yGlGrSWhEbSG3zapOQTPxO1RGhdSsIUd2y5uNQWa0bzFq4ciyMAjJOnF6jdXYMrgwmI6Oj42CUUAVAABntNYrW391eQAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMC0xMC0yOFQwMDoyOToxOSswMDowMDOfhXoAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjAtMTAtMjhUMDA6Mjg6MjMrMDA6MDCH/w5VAAAAAElFTkSuQmCC'

  line_count = 0
  lines.each { |line|  line_count += 1 unless line.start_with?('#') }
  if use_emoji_as_icon
    puts "💼#{line_count}"
  else
    puts "#{line_count} |templateImage=#{icon_base64}"
  end

  puts '---'

  cfn = File.expand_path(__FILE__)

  # Create the list of items to do in the menu.
  item_number = 0
  lines.each do |item|
    line_color = ''
    line = item.chomp
    if priority_labels.any? { |s| line.include? s }
      # If line contains priority label, display in priority color
      # line_color = priority_color
      # If line contains priority label, prefix item with priority_marker
      line = priority_marker + ' ' + line
    else
      # If line contains no priority label, cycle through labels hash,
      # and if line contains a label display in corresponding color
      labels.each { |label, label_color| line_color = label_color if line.include?(label) }
    end
    # If the line contains no label, display in default color. Otherwise, in
    # chosen color. Clicking line launches this script with line number as
    # the parameter.
    line_font = standard_font
    if line.start_with?('#')
      puts('---') unless line.start_with?('##')
      line_font = header_font
    end
    line_params = "#{line_color.empty? ? '' : 'color=' + line_color} #{line_font.empty? ? '' : 'font=' + line_font} bash='#{cfn}' param1=#{line_number[item_number]}"
    puts("#{line} | " + line_params + " param2=x terminal=false trim=false refresh=\n")
    puts("#{line} | alternate=true " + line_params + " param2=- terminal=false trim=false refresh=\n")
    item_number += 1
  end
  puts '---'
  puts "Click an item to mark as 'done'"
  puts "Click an item to mark as 'cancelled' | alternate=true"
  puts 'Refresh now (normally every 15m) | refresh='
else
  # This is what to do when clicking on an item. We want to move
  # the item to the Archive section and set it as done. If there
  # isn't an Archive area, create it and add the task to it.

  # Get the task number to archive.
  do_num = ARGV[0].to_i
  mark = ARGV[1]

  # Get the list of todos and setup variables
  todo_file = File.open(todo_file_loc.to_s)
  lines_in_file = IO.readlines(todo_file)

  unless lines_in_file[do_num].start_with?('#') # Do nothing if the item is a header
    task = ''
    lines = []
    line_number = 0

    lines_in_file[-1] = lines_in_file[-1] + "\n" unless lines_in_file[-1].include? "\n"

    # Process the todo list lines
    lines_in_file.each do |line|
      if line_number != do_num
        # It is one of the other lines. Just push it into the stack
        lines.push(line)
      else
        # Get the line to be moved to the archive area
        task = if insert_date_on_done_task
                 line.chomp + (mark == 'x' ? " @done(#{Time.new.strftime('%Y-%m-%d %H:%M')})\n" : "\n")
               else
                 task = line.chomp + "\n"
               end
        task = task.gsub(/^(\s*)([\-\*]+)\s*(\[ \]\s*)*/, '\1\2 [' + mark + '] ') # Works with both task style, useful if mix with 'show_alt_task', also it keeps the indentation at beginning of the line
        lines.push(task) if archive_task_at_end
      end
      line_number += 1
    end

    # Add the task to the bottom
    lines.push(task) if archive_task_at_end

    # Save the file
    IO.write(todo_file, lines.join)
  end
end
