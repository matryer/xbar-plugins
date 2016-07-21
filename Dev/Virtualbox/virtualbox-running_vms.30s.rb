#!/usr/bin/env ruby

# Show running Virtualbox VMs with option to shutdown (save state) using VBoxManage.
#
# <bitbar.title>Virtualbox running VMs</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>Harald Ringvold</bitbar.author>
# <bitbar.author.github>haraldringvold</bitbar.author.github>
# <bitbar.desc>Show running virtualbox VMs with option to shutdown (save state) using VBoxManage.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/YmFrYQH.png</bitbar.image>
# <bitbar.dependencies>Ruby,VBoxManage</bitbar.dependencies>


ENV['PATH'] = ENV['PATH']+':/usr/local/bin'

status = `VBoxManage list runningvms`

if status != ""
  vms = status.split(/\n/)

  print "🇻"
  print "#{vms.length}" if vms
  puts ""
  puts "---"

  vms.each do |vm|
    data_image = /('.*?'|".*?"|\S+).({.*?})/.match vm
    name = data_image[1].tr('"','')
    id = data_image[2].tr('{}','')

    puts "#{name}| color=black"
    puts "#{id}"
    puts "Save | color=green bash=VBoxManage  param1=controlvm param2=#{id} param3=savestate"
    puts "---"
  end
else
  # if no VMs are running
  puts "🇻"
end

