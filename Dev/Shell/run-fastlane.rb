#!/usr/bin/env ruby

# <xbar.title>Run fastlane</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Rishabh Tayal</xbar.author>
# <xbar.desc>Run fastlane from menu bar</xbar.desc>
# <xbar.author.github>rishabhtayal</xbar.author.github>
# <xbar.dependencies>bash</xbar.dependencies>

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
