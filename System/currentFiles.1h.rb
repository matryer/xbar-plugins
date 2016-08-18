#!/usr/bin/ruby
# coding: utf-8
#
# <bitbar.title>Current Working Files</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Richard Guay</bitbar.author>
# <bitbar.desc>List of files that I'm currently working on. It allows me to select which editor to use and integrates with Alfred workflow for BitBar.</bitbar.desc>

#
# Check for version of BitBar and set the check
# to be the normal check or an emoji check depending
# upon the version.
#
checkbeg = ""
checkin = ""
if ENV["BitBarVersion"] != nil
  #
  # The BitBar version is defined. It is at least version 2.0.0-beta10
  #
  checkbeg = ""
  checkin = "checked=true"
else
  #
  # This one is less than version 2.0.0-beta10
  #
  checkbeg = "✔️"
  checkin = ""
end

if ARGV.empty?
  puts '🗃';
  puts "---";
  puts "Files To Edit:"
  cfn = File.expand_path(__FILE__)
  IO.readlines(Dir.home + "/.myCurrentFiles").each { |i|
    fn = File.basename(i.chomp!)
    puts "#{fn} | bash=\"#{cfn}\" param1=\"#{i}\" terminal=false"
  }
  puts "---"
  puts "Editor to Use:"
  editor = IO.read(Dir.home + "/.myeditorchoice")
  if editor == "emacs"
    puts "#{checkbeg}Emacs Editor | bash=\"#{cfn}\" param1=\"emacs\" #{checkin} terminal=false refresh=true\n"
  else
    puts "Emacs Editor | bash=\"#{cfn}\" param1=\"emacs\" terminal=false refresh=true\n"
  end
  if editor == "sublime"
    puts "#{checkbeg}Sublime Text Editor | bash=\"#{cfn}\" param1=\"sublime\" #{checkin} terminal=false refresh=true\n"
  else
    puts "Sublime Text Editor | bash=\"#{cfn}\" param1=\"sublime\" terminal=false refresh=true\n"
  end
  if editor == "vim"
    puts "#{checkbeg}Vim Editor | bash=\"#{cfn}\" param1=\"vim\" terminal=false #{checkin} refresh=true\n"
  else
    puts "Vim Editor | bash=\"#{cfn}\" param1=\"vim\" terminal=false refresh=true\n"
  end
else
  case ARGV[0]
  when "emacs" then
    IO.write(Dir.home + "/.myeditorchoice","emacs")
  when "vim" then
    IO.write(Dir.home + "/.myeditorchoice","vim")
  when "sublime" then
    IO.write(Dir.home + "/.myeditorchoice","sublime")
  else
    fn = ARGV[0]
    if fn[0] == '~'
      fn = Dir.home + fn.slice(1,fn.length)
    end
    editor = IO.read(Dir.home + "/.myeditorchoice")
    case editor
    when "emacs" then
      `/usr/local/bin/emacsclient -n "#{fn}"`
    when "vim" then
      `'/usr/local/Cellar/macvim/7.4-106/MacVim.app/Contents/MacOS/MacVim' '#{fn}'`
    when "sublime" then
      `'/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl' '#{fn}'`
    end
  end
end
