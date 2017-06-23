#!/usr/bin/env ruby

# <bitbar.title>Heroku</bitbar.title>
# <bitbar.version>v0.2</bitbar.version>
# <bitbar.author>Deluan Quintao</bitbar.author>
# <bitbar.author.github>deluan</bitbar.author.github>
# <bitbar.desc>Currently this plugin displays Heroku service status and open issues</bitbar.desc>
# <bitbar.image>http://i.imgur.com/B7A1wvm.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>

require 'json'
require 'open-uri'

HEROKU_CMD="/usr/local/bin/heroku"

ICON="iVBORw0KGgoAAAANSUhEUgAAABQAAAAWCAYAAADAQbwGAAAAAXNSR0IArs4c6QAAAAlwSFlzAAALEwAACxMBAJqcGAAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAA3RJREFUOBGVVL9rFEEUfm9m93Y3xp/RQlGCiiBpBDs7ISRWsQtGELGwUNsoNkpOMGIRa/UfCMZgIShoFAkIESsrY6edFlGDJmfuZndm/N7s3ZkLCeq72915M+998837MUyrZHj4oSaax/N32f5xt1888IUXFsjNzlaLdTw8rzP531MtEPn6kcE7+5zzfcwO6NoKmlKKvbctO7LQlFNe1hzZBOqnqZdXP4gOYa5WqwqPE21kYGIyjtPTeVHPsRZhj7ARMXn8yjEWxA9rRaQrUeHMOxV19U8+vbQoIVPv3/eJIQkwKdqtVSxqHOmY4yglrSJ4gyYr/LAaJZhPxCaCLnvuybneJRMLC/OsZBCkirenFetAzlNurbltisaQtcUsoMh7Z523zhT1azIPo1esxN0v6UYeQrBrV5//AyhLXqhgV6bYOX48NTP6BPTmlYqEqWbiZ1MzV8aNbbyB0SG4iHRURQdguR42I9auR3RoEcBC2Dy552f7b/UkOp5EOPblRQMWwuCPrANYLrYyCbaZMLQ2/4xcnDQ6fh1H2SASV7DkZo1sCFiwK3f2fG+5sTjAho4YZc+hkM6bYuURkgTaIqFAyiHeGwCy18R1sfKWvqLWUl/hc0nBBx/MjM6Ri64jST/KiuBQry3E5i6lGpIWhh6txGdODU5cAtgJrZIsS7bQcv3bOywfjWKzZK1eQva3QkeYQ9UEzw5A75mlguUfV7KzYCGxI5RSUTfLYlsTL89awqFhJqaI5C+ZDrLmyBIPaQRvTV5bdK6Q+sOcdyEBzXDlpmhilSCr3x2AYAiOkjmpZHUdRX0vdJnoIs206ijUb0gxXgLelg5A2OPEaFrmCrzfPnhx+SIYzknDBaYIqHhqVnJddQC1ENuAYzTmIWgGaTNvmOxhXBbjMDyGGOLIMMUJxNEwh/svUJQQrZJ2Um5UbzDPdaNh0cvkV8DzplK6F/0r5nWkJcN3/8iJibtkSbKblp1SshYjkTbDanUMu/OWtNKNXku2punmXq1jXDspVeIsk29S2bR3c7bzQnfacxo3zo6k0iX8eqyNA025baLp6eFm7iRAE09qtW/bcfClwppwfslTaAbZGklvmJocNwCA9TYMX9MKfRd2x4+TawVAviE+Q0P3u7Y1frbmxW5DqZsaT8+OoTbL2K41/CeQtU5Nve37GzcPb+4ia8bVAAAAAElFTkSuQmCC"

def status_icon(icon="", ok=true)
  if icon.respond_to?('each')
    msg = ""
    icon.each do |i|
      msg += "#{i} | image=#{ICON} dropdown=false\n"
    end
  else
    msg = "#{icon} | image=#{ICON} dropdown=false\n"
    msg += "#{icon} | image=#{ICON} dropdown=false color=red\n" unless ok
  end
  msg + "---"
end

def status
  begin
    @status ||= (content = URI("https://status.heroku.com/api/v3/current-status").read; JSON.parse(content))
  rescue => e
    puts status_icon ":interrobang:", false
    puts "Error: #{e} | color=red"
    puts "---"
    puts "Refresh... | refresh=true"
    exit
  end
end

def issues
  @issues ||= [].tap do |resp|
    status["issues"].each do |issue|
      href = issue["full_url"]
      title = issue["title"]
      color = issue["status_prod"]
      resp << "#{title} | color=#{color} href=#{href}"
    end
  end
end

if status["status"]["Production"] != "green"
  puts status_icon [":grey_exclamation:", ":exclamation:"], false
else
  puts status_icon
end

puts "---"
if issues.size > 0
  issues.each do |issue|
    puts issue
  end
else
  puts "All good. Relax | href=https://status.heroku.com/ color=green"
end

puts "---"
puts "Refresh... | refresh=true"
