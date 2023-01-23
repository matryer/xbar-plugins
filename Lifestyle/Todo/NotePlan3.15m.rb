#!/usr/bin/env ruby
# coding: utf-8

# <xbar.title>Todo Today for NotePlan v3</xbar.title>
# <xbar.version>v2.2</xbar.version>
# <xbar.author>Jonathan Clark</xbar.author>
# <xbar.author.github>jgclark</xbar.author.github>
# <xbar.desc>A todo list taken from NotePlan v3 and displayed with customizable color-code. Mark tasks "done" simply by clicking on them in the menubar drop-down list. This was based on "Todo.NotePlan" by Richard Guay which in turn was based on "Todo Colour" plugin by Srdgh.</xbar.desc>
# <xbar.dependencies>ruby</xbar.dependencies>
# <xbar.image>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</xbar.image>
# <xbar.abouturl>https://noteplan.co/</xbar.abouturl>
#
# Modifications by Jonathan Clark
#   v2.2, 2021/01/29:
#     - tweak default 'priority_label' to suit planned change in NP3
#   v2.1, 2020/11/29:
#     - auto-detect storage type (CloudKit > iCloud Drive > Drobpox if there are multiple)
#     - add option to specify the file extension in use (default to md, but can be txt)
#   v2.0, 2020/10/30:
#     - Update NP data storage filepaths for NotePlan 3 beta
#       (including CloudKit change at v3.0.15 beta)
#     - Make CloudKit location the default
#     - tweak colours and falgs to suit my needs
#     - ignore tasks with dates scheduled into the future
#     - improve some non-tasks it was including
#     - code clean up
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
file_extension = '.md'           # Defaults to file extension type 'md' -- can change to '.txt'
priority_labels = ['@urgent', '#high', '#⭐️']
priority_marker = '⭐'
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
