#!/usr/bin/env ruby

# <bitbar.title>Run fastlane</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Rishabh Tayal</bitbar.author>
# <bitbar.desc>Run fastlane from menu bar</bitbar.desc>
# <bitbar.author.github>rishabhtayal</bitbar.author.github>
# <bitbar.dependencies>bash</bitbar.dependencies>

# ------------- Modify the config below -------------
configs = [
  {
    'lane_name' => 'YOUR_LANE_NAME',
    'path' => 'YOUR_PROJECT_PATH',
    'use_bundle' => true
  },
  {
    'lane_name' => 'YOUR_LANE_NAME',
    'path' => 'YOUR_ANOTHER_PROJECT_PATH',
    'use_bundle' => true
  }
  # YOU CAN ADD MORE CONFIG MAPS HERE
]

# ------------ Ignore below this line ---------------

require 'pathname'

puts 'run-fastlane'

puts '---'

configs.each do |item|
  command = "cd #{item['path']} && "
  command << if item['use_bundle']
               'bundle exec fastlane'
             else
               'fastlane'
             end
  command << ' ' + item['lane_name']

  project_name = Pathname.new(item['path']).split.last.to_s
  puts "#{project_name}: #{item['lane_name']} | bash=\"#{command}\""
end
