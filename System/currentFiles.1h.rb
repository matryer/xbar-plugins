#!/usr/bin/ruby
# coding: utf-8
#
# <bitbar.title>Current Working Files</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Richard Guay</bitbar.author>
# <bitbar.author.github>raguay</bitbar.author.github>
# <bitbar.desc>List of files that I'm currently working on. It allows me to select which editor to use and integrates with Alfred workflow for BitBar.</bitbar.desc>
# <bitbar.image>http://customct.com/images/CurrentFilesPlugin-01.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.abouturl>http://customct.com/bitbar</bitbar.abouturl>

#
# Global Variables:
#
checkbeg = ""
checkin = ""
vimProg = "/usr/local/Cellar/macvim/7.4-107/MacVim.app/Contents/MacOS/MacVim"
sublimeProg = "/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl"
emacsProg = "/usr/local/bin/emacsclient"

#
# Check for needed files existing and create them
# if they don't.
#
if ! File.exist?(Dir.home + "/.myCurrentFiles")
  fh = File.new(Dir.home + "/.myCurrentFiles","w+")
  fh.write("~/.zshrc\n~/.bashrc\n~/.zshenv\n~/.zlogin\n~/.profile\n~/.vimrc\n")
  fh.close
  fh = File.new(Dir.home + "/.myeditors","w+")
  fh.write("Emacs|emacs\nMacVim|vim\nSublime Text|sublime\n")
  fh.close
  fh = File.new(Dir.home + "/.myeditorchoice","w+")
  fh.write("sublime")
  fh.close
end

#
# Check for version of BitBar and set the check
# to be the normal check or an emoji check depending
# upon the version.
#
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
  #
  # Create the menu.
  #
  puts '🗃';
  puts "---";
  puts "Files To Edit:"
  cfn = File.expand_path(__FILE__)
  IO.readlines(Dir.home + "/.myCurrentFiles").each { |i|
    fn = File.basename(i.chop!)
    puts "#{fn} | bash=\"#{cfn}\" param1=\"file\" param2=\"#{i}\" terminal=false"
  }
  puts "---"
  puts "Editor to Use:"
  editor = IO.read(Dir.home + "/.myeditorchoice")
  IO.readlines(Dir.home + "/.myeditors").each { |i|
    if i.chop! == ""
        continue
    end
    parts = i.split("|")
    parts[1]
    if editor == parts[1]
      puts "#{checkbeg}#{parts[0]} | bash=\"#{cfn}\" param1=\"#{parts[1]}\" #{checkin} terminal=false refresh=true\n"
    else
      puts "#{parts[0]} | bash=\"#{cfn}\" param1=\"#{parts[1]}\" terminal=false refresh=true\n"
    end
  }
else
  #
  # Process a command.
  #
  case ARGV[0]
  when "file" then
    #
    # Open the file in the preferred editor.
    #
    fn = ARGV[1]
    if fn[0] == '~'
      fn = Dir.home + fn.slice(1,fn.length)
    end
    editor = IO.read(Dir.home + "/.myeditorchoice")
    case editor
    when "emacs" then
      #
      # Call the emacsclient program to open the file.
      #
      `#{emacsProg} -n "#{fn}"`
    when "vim" then
      #
      # Call MacVim to open the file.
      #
      `'#{vimProg}' '#{fn}'`
    when "sublime" then
      #
      # Call Sublime Text to open the file.
      #
      `'#{sublimeProg}' '#{fn}'`
    else
      #
      # Else, let the system open the file with the
      # desired program.
      #
      `/usr/bin/open -a '#{editor}' '#{fn}'`
    end
  else
    #
    # Set the editor Choice.
    #
    IO.write(Dir.home + "/.myeditorchoice",ARGV[0])
  end
end
