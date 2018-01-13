#!/usr/bin/env ruby
# coding: utf-8

# <bitbar.title>Agenda</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>yqrashawn</bitbar.author>
# <bitbar.author.github>yqrashawn</bitbar.author.github>
# <bitbar.desc>display emacs org-agenda in bitbar</bitbar.desc>
# <bitbar.image>https://github.com/yqrashawn/bitbar-plugin-agenda/raw/master/bitbar-ext-org-agenda.png</bitbar.image>
# <bitbar.dependencies>ruby,emacs</bitbar.dependencies>
# <bitbar.abouturl>http://yqrashawn.com/2017/11/25/org-agenda-bitbar-plugin/</bitbar.abouturl>

# for more information please checkout http://yqrashawn.com/2017/11/25/org-agenda-bitbar-plugin/
# it's on github https://github.com/yqrashawn/bitbar-plugin-agenda

require 'open3'

Encoding.default_external = Encoding::UTF_8
Encoding.default_internal = Encoding::UTF_8

# your exported agenda files directory path eg. "#{Dir.home}/agendas/work/"
agenda_directory = 'your agenda directory'

# the exported txt agenda file that in the agenda_directory eg. 'todos.txt'
agenda_name = 'todos.txt'

# the agenda custome command which brings the agenda view that you want to export
agenda_custome_command = 'B'

# function to REALLY kill emacs
# for spacemacs use (spacemacs/kill-emacs)
kill_emacs_function = '(let (kill-emacs-hook) (kill-emacs))'

# Change priority color here
tag_color = 'orange'

# Customise label color-code here (these colors are optimised for a dark theme menubar)
labels = {
  '[#A]' => 'red',
  '[#B]' => 'yellow',
  '[#C]' => 'violet'
}

tag_indicator = 'Headlines with TAGS match: '

# close stdout stderr
system '/usr/local/bin/emacs',
       '--batch',
       '-l',
       '~/.emacs.d/init.el',
       '--eval',
       '(run-hooks \'emacs-startup-hook)',
       '--eval',
       "(progn (org-agenda nil \"#{agenda_custome_command}\") (org-agenda-write \"#{agenda_directory}#{agenda_name}\") #{kill_emacs_function})",
       :out => :close,
       :err => :close

agenda_file = File.open("#{agenda_directory}#{agenda_name}")

lines = IO.readlines(agenda_file)

# remove empty line
lines.reject! { |s| s.nil? || s.strip.empty? }
lines.reject! { |s| s.include?('=====') }
lines.each.with_index do |line, i|
  # get url for urls [[https://example.com][example]]
  url = ''
  if line.include?('[[http')
    url = line.slice(/\[\[((http?|ftp).*\]\[)/)
    lines[i] = line.sub(url, '')
    lines[i] = lines[i].sub ']]', ''
    line = lines[i]
    url = url.slice(2, url.length - 4)
  end

  # detect tag line
  if line.include?(tag_indicator)
    lines[i] = "#{line.slice(tag_indicator.length, line.length).delete("\n")} | color=#{tag_color} font=Hack"
  else
    # get color dpends on priority
    line_color = ''
    labels.each { |label, label_color| line_color = label_color if line.include?(label) }
    line_color = 'white' if line_color.strip.empty?

    # remove TODO, add color, special font for clickable one
    lines[i] = "#{line.delete("\n").squeeze(' ')}|color=#{line_color} #{url.strip.empty? ? '' : "font=Hack bash=open param1=" + "'" + url + "'" + " terminal=true"}"
  end
end

puts "Do: #{lines.length}"
puts '---'
puts lines