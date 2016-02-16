#!/usr/bin/env ruby
# <bitbar.title>Save to Linkpack</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Taylor Zane Glaeser</bitbar.author>
# <bitbar.author.github>taylorzane</bitbar.author.github>
# <bitbar.desc>Saves the current Safari link to the selected Linkpack folder. Created because Linkpack lacks a Safari extension, for now. Theoretically this doesn't need to be used with Linkpack, it will just save a bookmark to disk.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/MasCD4V.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.abouturl>https://www.taylorzane.com</bitbar.abouturl>

# --------------------- #
# EDIT THESE VARIABLES. #
# --------------------- #

MENU_TEXT = 'Save to Linkpack'
LINKPACK_DIR = '/full/path/to/link/pack/dir'

# -------------------------------------------------------- #
# DON'T EDIT BELOW HERE UNLESS YOU KNOW WHAT YOU'RE DOING. #
# -------------------------------------------------------- #

LINKPACK_FOLDERS = Dir["#{LINKPACK_DIR}/**/*/"]

if ARGV[0] == 'save'
  PAGE_TITLE = `osascript -e 'tell Application "Safari"' -e 'name of current tab of window 1' -e 'end tell'`
  PAGE_URL = `osascript -e 'tell Application "Safari"' -e 'URL of current tab of window 1' -e 'end tell'`

  SAVE_FOLDER = ARGV[1].gsub(/ZZspaceZZ/, ' ')

  File.open(SAVE_FOLDER + (PAGE_TITLE.strip || PAGE_URL.chomp('/')) + '.url', 'w') do |file|
    file.write "[InternetShortcut]\n"
    file.write "URL=#{PAGE_URL.strip}\n"
    file.write "TITLE=#{PAGE_TITLE.strip}\n"
  end
else
  puts MENU_TEXT
  puts "---"

  LINKPACK_FOLDERS.each do |folder|
    folder_display = folder.split(LINKPACK_DIR)[1].gsub(/^\/|\/$/, '')

    puts "#{folder_display} | bash=#{$0} param1=save param2=#{folder.gsub(/\s/, 'ZZspaceZZ')} terminal=false"
  end

  puts "---"
  puts "Refresh Folders | refresh=true"
  # NOTE: This cannot have terminal=false or it won't trigger.
  # cont: I didn't want to make an ugly AppleScript command.
  # cont: I will update this if enough people decide they
  # cont: don't like the Terminal window popping up.
  puts "Open Directory | bash=open param1=#{LINKPACK_DIR}"
end
