#!/usr/bin/env ruby

# Show running Virtualbox VMs with option to shutdown (save state) using VBoxManage.
#
# <xbar.title>Virtualbox running VMs</xbar.title>
# <xbar.version>1.0</xbar.version>
# <xbar.author>Harald Ringvold</xbar.author>
# <xbar.author.github>haraldringvold</xbar.author.github>
# <xbar.desc>Show running virtualbox VMs with option to shutdown (save state) using VBoxManage.</xbar.desc>
# <xbar.image>http://i.imgur.com/YmFrYQH.png</xbar.image>
# <xbar.dependencies>Ruby,VBoxManage</xbar.dependencies>


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

