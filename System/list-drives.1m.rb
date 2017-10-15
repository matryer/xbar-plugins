#!/usr/bin/env ruby

# <bitbar.title>Show info about hard drives</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Artem Kramarenko</bitbar.author>
# <bitbar.author.github>artemk</bitbar.author.github>
# <bitbar.desc>Show info about hard drives and free space left. Alert if free space is below 10%</bitbar.desc>
# <bitbar.image>http://bit.ly/2zpBJu6</bitbar.image>
# <bitbar.dependencies>OSX, ruby, gem install plist</bitbar.dependencies>

require 'plist'

alert_free_space_percentage = 0.1 #10%

def volume_info(list_of_volumes, name)
  list_of_volumes["AllDisksAndPartitions"].detect{|v| v["VolumeName"] == name} ||
    list_of_volumes["AllDisksAndPartitions"].map{|r| r["Partitions"]}.detect{|v| v["VolumeName"] == name}
end

begin
  puts "💽"
  puts "---"
  puts "Refresh | refresh=true"
  puts "---"
  diskutil_list_pist = Plist.parse_xml(`diskutil list -plist`)
  whole_disks = diskutil_list_pist["WholeDisks"]
  volumes_from_disks = diskutil_list_pist["VolumesFromDisks"]

  metadata = {}
  whole_disks.each do |disk|
    disk_data = Plist.parse_xml(`diskutil info -plist '#{disk}'`)
    data = {
      "top" => (disk_data["VirtualOrPhysical"] == "Physical"),
      "tree" => disk_data["DeviceTreePath"],
      "total_size" => disk_data["TotalSize"],
      "ejectable" => disk_data["Ejectable"],
      "name" => disk_data["IORegistryEntryName"],
      "internal" => disk_data["Internal"],
      "volumes" => []
    }
    metadata[disk] = data
  end

  volumes_from_disks.each do |volume_name|
    volumes_data = Plist.parse_xml(`diskutil info -plist '#{volume_name}'`)
    data = {
      "tree" => volumes_data["DeviceTreePath"],
      "ejectable" => volumes_data["Ejectable"],
      "name" => volumes_data["VolumeName"],
      "internal" => volumes_data["Internal"],
      "mount" => volumes_data["MountPoint"],
      "free_space" => volumes_data["FreeSpace"],
      "volume_space" => volumes_data["VolumeSize"]
    }
    if volumes_data["WholeDisk"]
      disk = metadata.detect{|k,v| v["tree"] == data["tree"]}.first
    else
      disk = volumes_data["ParentWholeDisk"]
    end
    metadata[disk]["volumes"] << data
  end


  metadata.select{|k,v| v["top"]}.each do |disk_name, metadata|
    disk_puts = ""
    disk_puts += "⏏️ " if metadata["ejectable"]
    disk_puts += "#{metadata["name"]} (#{disk_name}) | color='black' size=14"
    puts disk_puts

    metadata["volumes"].each do |volume|
      volume_size_gb = (volume["volume_space"] / 1073741824.0).round(2)
      free_space_db = (volume["free_space"] / 1073741824.0).round(2)

      color_name = (volume["free_space"].to_f / volume["volume_space"].to_f) < alert_free_space_percentage ? 'red' : 'gray'
      puts "- #{volume['name']} (Total:#{volume_size_gb}GB / Free:#{free_space_db}GB) | color=#{color_name} href=file://#{volume['mount']}"
    end
    puts "---"
  end
rescue StandardError => err
  puts "🚩"
  puts "---"
  puts err.backtrace
end

