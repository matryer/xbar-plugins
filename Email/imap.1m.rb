#!/usr/bin/env ruby 

# Metadata allows your plugin to show up in the app, and website.
#
#  <xbar.title>Check IMAP</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Maesitos</xbar.author>
#  <xbar.author.github>https://github.com/maesitos</xbar.author.github>
#  <xbar.desc>Fetch unread from any IMAP mail</xbar.desc>
#  <xbar.dependencies>ruby</xbar.dependencies>
#  <xbar.image>https://user-images.githubusercontent.com/3597038/161608594-8ba677ac-c257-4f9a-a853-a224402ca06a.png</xbar.image>

# Variables become preferences in the app:
#
#  <xbar.var>string(IMAP_URL=""): The URL where the server is hosted</xbar.var>
#  <xbar.var>string(IMAP_USER=""): The user of the IMAP server</xbar.var>
#  <xbar.var>string(IMAP_PASS=""): Password for the IMAP server</xbar.var>
#  <xbar.var>string(WEBMIL_URL=""): The URL where you'll be checking the email</xbar.var>

require 'net/imap'

# Art
filled_logo = "iVBORw0KGgoAAAANSUhEUgAAABQAAAAUCAYAAACNiR0NAAAABmJLR0QA/wD/AP+gvaeTAAAA/ElEQVQ4jeXUvUoDQRSG4SdBEOwtvArtcgm2FtqI4OIPKLaCNnaW6Uwhiq0XYKfXYKFegAgpDGipMQhxLTwL67ILusYqB4Zhvjnfy3DmzDB20Yh5FtN/ZD3hFlpYQBdpzdENRgsSPGMJ1zVgd1hEL1iS2BhgAxe/gF2G/zXWSTNXg0mc4AZHP6jZKa5whqlMbBaSGjiIhF18lIBSHOId7SKjCMxiHfNYQz+nD7CJOeyUGauAMAzAiq+C97CMF0xUmao2jnGPczxiO/QOZrCPB2yVmbNbTuNUe2HM32Y/Rl7rRO4wp31rmzf12mZVrm0y4Egbe6RP718+hzGLT5ackdTUECTiAAAAAElFTkSuQmCC"
empty_logo = "iVBORw0KGgoAAAANSUhEUgAAABQAAAAUCAYAAACNiR0NAAAABmJLR0QA/wD/AP+gvaeTAAABGUlEQVQ4jeXUTSuEURTA8d88pEl2s6EsZK/Id2BlJRZTMiWaBSnJys4CZWNhkCiSBb4AGzs78SFIKVtSGhZzp56envEyMxs5dTu38/K/95z7wr+TTNADyDXIesZtBsM4xSbu64R1Yx5jUMBogA7WAevDGcZRiGLbnUARI7+ADWEBk3iCKOZ8wwz6MfcD2HQATuGlaowSQR9YCQEbKX4qB7mMNiyiHHemJcA+LnCA9pg9iz3cYSstsRYQWgLgGJ1hnKAD77WSWmvYi+hFHl3YDvZZPGINPdj5bocRVoNeUunPQwDnw7wcfPHYVGAWu7hGKbHQaxhxKeFKos/VknM4wjpukmV8IZcqLTjEOZUr0NSn1/TPoUHGX5RP4ys21zjkStQAAAAASUVORK5CYII="

imap = Net::IMAP.new(ENV['IMAP_URL'], ssl: true)
begin
    imap.authenticate('PLAIN', ENV['IMAP_USER'], ENV['IMAP_PASS'])
    #Try Login if if doesn't work
rescue
    abort 'Authentication failed'
end

imap.examine('INBOX')
unread = imap.status("inbox", ["UNSEEN"])["UNSEEN"]

if unread > 0
  puts "#{unread} | templateImage=#{filled_logo}"
else  
  puts " | templateImage=#{empty_logo}"
end
unless ENV['WEBMIL_URL'].empty?
  puts "---"
  puts "Read e-mail | href=\"#{ENV['WEBMIL_URL']}\""
end
