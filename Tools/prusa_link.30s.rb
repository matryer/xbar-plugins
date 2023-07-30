#!/usr/bin/env ruby

# <xbar.title>PrusaLink Monitor</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Sebastian Ludwig</xbar.author>
# <xbar.author.github>sebastianludwig</xbar.author.github>
# <xbar.desc>Show the status of a Prusa 3D printer</xbar.desc>
# <xbar.dependencies>ruby</xbar.dependencies>

# <xbar.var>string(PRINTER_ADDRESS=""): Address of the PrusaLink web interface</xbar.var>
# <xbar.var>string(API_KEY=""): API key of your printer</xbar.var>

require "uri"
require "net/http"
require "json"
require "shellwords"

API_KEY = ENV["API_KEY"]
BASE_URI = ENV["PRINTER_ADDRESS"]

def json_api_request(path)
  uri = URI.join BASE_URI, path    
  Net::HTTP.start(uri.host, uri.port, use_ssl: uri.scheme == "https") do |http|
    request = Net::HTTP::Get.new uri.request_uri
    request["x-api-key"] = API_KEY

    response = http.request(request)
    break nil if not response.is_a?(Net::HTTPSuccess)

    begin
      json = JSON.parse!(response.body)
    rescue
      break nil
    end

    json
  end
end

def start_submenu
  puts "---"
  puts "Copy API key | shell=/bin/sh | param1='-c' | param2='echo #{API_KEY} | pbcopy'"
  puts "Refresh | refresh=true"
end

begin
  printer_json = json_api_request "api/printer"

  temp_nozzle = printer_json.dig("telemetry", "temp-nozzle")
  temp_bed = printer_json.dig("telemetry", "temp-bed")

  job_status_json = json_api_request "api/job"

  print_time_left_seconds = job_status_json.dig("progress", "printTimeLeft")
  completion = job_status_json.dig("progress", "completion")

  menu_bar_content = "%.0f/%.0f°C" % [temp_nozzle, temp_bed]
  menu_bar_content += " %3d%%" % (completion * 100) if completion
  menu_bar_content += "/-%dm" % (print_time_left_seconds / 60) if print_time_left_seconds

  puts menu_bar_content

  # Context menu

  temp_nozzle_target = printer_json.dig("temperature", "tool0", "target")
  temp_bed_target = printer_json.dig("temperature", "bed", "target")
  print_time_seconds = job_status_json.dig("progress", "printTime")

  start_submenu

  puts "State                 %s" % printer_json.dig("state", "text")
  puts "Nozzle               %3.0f °C" % temp_nozzle
  puts "Nozzle               %3.0f °C / %3.0f °C | alternate=true" % [temp_nozzle, temp_nozzle_target]
  puts "Bed                     %3.0f °C" % temp_bed
  puts "Bed                     %3.0f °C / %3.0f °C | alternate=true" % [temp_bed, temp_bed_target]
  puts "Material              %s" % printer_json.dig("telemetry", "material")

  puts "Print speed        %3d %%" % printer_json.dig("telemetry", "print-speed")

  puts "Progress              %3d %%" % (completion * 100) if completion
  puts "Z-height              %3.1f mm" % printer_json.dig("telemetry", "z-height")
  puts "Print time             %3d m" % (print_time_seconds / 60) if print_time_seconds
  puts "Print time left      %3d m" % (print_time_left_seconds / 60) if print_time_left_seconds
rescue => e
  puts "🤷‍♂️"
  
  start_submenu

  puts "Error: #{e} | shell=/bin/sh | param1='-c' | param2='echo #{Shellwords.escape(e)} | pbcopy'"
end
