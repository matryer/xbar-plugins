#!/usr/bin/env ruby
# coding: utf-8
# frozen_string_literal: true

#  <xbar.title>Hololive</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Daniils Petrovs</xbar.author>
#  <xbar.author.github>danirukun</xbar.author.github>
#  <xbar.desc>Display live and upcoming Hololive streams in your menu bar!</xbar.desc>
#  <xbar.image>https://i.imgur.com/BEOzDBM.png</xbar.image>
#  <xbar.dependencies>ruby</xbar.dependencies>
#  <xbar.abouturl>https://github.com/DaniruKun/hololive-xbar-plugin</xbar.abouturl>
#  <xbar.var>boolean(VAR_VERBOSE=false): Whether to be verbose or not.</xbar.var>
#  <xbar.var>boolean(VAR_SHOW_TIME=true): Whether to show time to upcoming streams or time since start.</xbar.var>
#  <xbar.var>select(VAR_STYLE="normal"): Which style to use. [small, normal, big]</xbar.var>

# Hololive xbar plugin.
# Copyright (C) 2021 Daniils Petrovs
# https://github.com/DaniruKun

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

require 'json'
require 'uri'
require 'net/http'
require 'openssl'
require 'time'

Encoding.default_external = Encoding::UTF_8
Encoding.default_internal = Encoding::UTF_8

# Plugin icon as base64 string
IMG_BASE64 = 'iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAAXNSR0IArs4c6QAAAJZlWElmTU0AKgAAAAgABQESAAMAAAABAAEAAAEaAAUAAAABAAAASgEbAAUAAAABAAAAUgExAAIAAAARAAAAWodpAAQAAAABAAAAbAAAAAAAAACQAAAAAQAAAJAAAAABd3d3Lmlua3NjYXBlLm9yZwAAAAOgAQADAAAAAQABAACgAgAEAAAAAQAAABigAwAEAAAAAQAAABgAAAAAXjAL1AAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAActpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDYuMC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOnhtcD0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wLyI+CiAgICAgICAgIDx0aWZmOk9yaWVudGF0aW9uPjE8L3RpZmY6T3JpZW50YXRpb24+CiAgICAgICAgIDx4bXA6Q3JlYXRvclRvb2w+d3d3Lmlua3NjYXBlLm9yZzwveG1wOkNyZWF0b3JUb29sPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4K56DsKAAAAVlJREFUSA21ljtOxDAQhsNLVAiEOABUHAEKxCEoKLgGQlyAlpaKC9Cj5VHTUCJOgUAUdIjn9y/50ewGr2KcjPSt7SSePzMeO1tVP7ZMcwYvcA07MAuyGZga9gp+jpj7BR91q/4AtsE2TUdk2xwzzkFO30Ai7/X4lfYUVsGm57MiWmLCDUjAji2mVjzDAehZmQRaR7PIw38JyPEnxLTdMd4DW6u0TRJwBBJS+jy+or9pFVoVRDKiVIrsLLYxheqfwBrYVHENyxGQ2HjaHrh2CAvB80g0uQKpiO4R2A0iv9GUCEgslrXGF7BeCw0jKRVwRBJS+jR+hA1Ir7xuZpreVvtDm3MF9qFTAflr2MhqN+7mXXB65pn2BMea3oWAy9YpusTvFtzaf8kix43XWZn6jV09nW60+Matj4reD7tJAuPp+NdxnVrkeDwXfXB6/2RSrlWvH30J9Pa35RsDtfNl2vuLzQAAAABJRU5ErkJggg=='

UTC_NOW = Time.now.utc
SHOW_TIME_DIFF = ENV['VAR_SHOW_TIME'] || 'true'
MAX_TITLE_LEN = 30
ENTRY_WIDTH = 50
HOLODEX_URL = 'https://holodex.net'

class Video
  def initialize(title, yt_video_key, channel_id, scheduled_start, live_start)
    @title = title.gsub('|', '')
    @yt_video_key = yt_video_key
    @channel_id = channel_id
    @scheduled_start = scheduled_start
    @live_start = live_start
  end

  def to_entry_str
    fanmark = Hololive.channel_emoji @channel_id
    formatted_title = @title.length > MAX_TITLE_LEN ? @title.slice(0..MAX_TITLE_LEN).concat('...') : @title

    case SHOW_TIME_DIFF
    when 'true'
      "#{fanmark} #{formatted_title} #{formatted_time_diff} |" \
      " href=https://youtu.be/#{@yt_video_key}"
    when 'false'
      "#{fanmark} #{formatted_title} |" \
      " href=https://youtu.be/#{@yt_video_key}"
    end
  end

  private

  def formatted_time_diff
    start_time = Time.parse(@live_start || @scheduled_start)
    seconds_diff = (start_time - UTC_NOW).to_i.abs

    hours = seconds_diff / 3600
    seconds_diff -= hours * 3600
    minutes = seconds_diff / 60

    if hours.positive?
      "[#{hours}h #{minutes}m]"
    else
      "[#{minutes}m]"
    end
  end
end

# rdoc
#   Class encapsulating main plugin logic related to API calls and printing to stdout.
class Hololive
  def initialize
    lives = holofans_api('https://api.holotools.app/v1/videos?limit=20&order=asc&sort=live_start&status=live&with_comments=0')
    @videos_live = lives.map do |v|
      Video.new(v['title'], v['yt_video_key'], v['channel']['yt_channel_id'], v['live_schedule'], v['live_start'])
    end
    upcoming = holofans_api('https://api.holotools.app/v1/videos?limit=30&status=upcoming&order=asc&sort=live_schedule')
    @videos_upcoming = upcoming.map do |v|
      Video.new(v['title'], v['yt_video_key'], v['channel']['yt_channel_id'], v['live_schedule'], v['live_start'])
    end
  end

  def holofans_api(resource)
    url = URI.parse(resource)
    http = Net::HTTP.new(url.host, url.port)
    http.use_ssl = true
    request = Net::HTTP::Get.new(url.request_uri)
    response = http.request(request)
    Array(JSON.parse(response.body)['videos'])
  end

  def print_data
    # Print menu bar icon
    puts "| size=14 color=#BFBFBF trim=false templateImage=#{IMG_BASE64}"
    puts '---'
    print_live
    print_upcoming
    puts "Open in Holodex | href=#{HOLODEX_URL}"
  end

  def self.channel_emoji(yt_channel_id)
    channel_emoji = {
      # 0th Generation
      'UCp6993wxpyDPHUpavwDFqgg' => '🐻',
      'UCDqI2jOz0weumE8s7paEk6g' => '🤖',
      'UC-hM6YJuNYVAmUWxeIr9FeA' => '🌸',
      'UC5CwaMl1eIgY8h02uZw7u8A' => '☄️',
      'UC0TXe_LYZ4scaW2XMyi5_kw' => '⚒️',
      # 1st Generation
      'UCD8HOxPs4Xvsm8H0ZxXGiBw' => '🌟',
      'UC1CfXB_kRs3C-zaeTG3oGyg' => '♥️',
      'UCdn5BQ06XqgXoAxIhbqw5Rg' => '🌽',
      'UCQ0UDLQCjY0rmuxCDE38FGg' => '🏮',
      'UCLbtM3JZfRTg8v2KGag-RMw' => '🍎',
      # 2nd Generation
      'UC1opHUrw8rvnsadT-iGp7Cg' => '⚓',
      'UCXTpFs_3PqI41qX2d9tL2Rw' => '🌙',
      'UC7fk0CB07ly8oSl0aqKkqFg' => '😈',
      'UC1suqwovbL1kzsoaZgFZLKg' => '💋',
      'UCvzGlP9oQwU--Y0r9id_jnA' => '🚑',
      # Hololive GAMERS
      'UCp-5t9SrOQwXMU7iIjQfARg' => '🌲',
      'UCvaTdHTWBGv3MKj3KVqJVCw' => '🍙',
      'UChAnqc_AY5_I3Px5dig3X1Q' => '🥐',
      # 3rd Generation
      'UC1DCedRgGHBdm81E1llLhOQ' => '👯',
      'UCl_gCybOJRIgOXw6Qb4qJzQ' => '🦋',
      'UCvInZx9h3jC2JzsIzoOebWg' => '🔥',
      'UCdyqAaZDKHXg4Ahi7VENThQ' => '⚔️',
      'UCCzUftO8KOVkV4wQG1vkUvg' => '🏴‍☠️',
      # 4th Generation
      'UCZlDXzGoo7d44bwdNObFacg' => '💫',
      'UCS9uQI-jC3DE0L4IpXyvr6w' => '🐉',
      'UCqm3BQLlJfvkTsX_hvm0UmA' => '🐏',
      'UC1uv2Oq6kNxgATlCiez59hw' => '👾',
      'UCa9Y57gfeY0Zro_noHRVrnw' => '🍬',
      # 5th Generation
      'UCFKOVgVbGmX65RxO3EtH3iw' => '☃️',
      'UCAWSyEs_Io8MtpY3m-zqILA' => '🥟',
      'UCUKD-uaobj9jiqB-VXt71mA' => '♌',
      'UCK9V2B22uJYu3N7eR_BT9QA' => '🎪',
      # Holostars
      'UC6t3-_N8A6ME1JShZHHqOMw' => '🌺',
      'UCZgOv3YDEs-ZnZWDYVwJdmA' => '🎸',
      'UCKeAhJvy8zgXWbh9duVjIaQ' => '🍕',
      'UC9mf_ZVpouoILRY9NUIaK-w' => '⚙️',
      'UCNVEsYbiZjH5QLmGeSgTSzg' => '🎭',
      'UCGNI4MENvnsymYjKiZwv9eg' => '🦔',
      'UCANDOlYTJT7N5jlRC3zfzVA' => '🍷',
      'UChSvpZYRPh0FvG4SJGSga3g' => '🟣',
      'UCwL7dgTxKo8Y4RFIKWaf8gA' => '🐃',
      # HoloID
      'UCOyYb1c43VlX9rc_lT6NKQw' => '🐿',
      'UCP0BspO_AMEe3aQqqpo89Dg' => '🔮',
      'UCAoy6rzhSf4ydcYjJw3WoVg' => '🎨',
      'UCYz_5n-uDuChHtLo7My1HnQ' => '🧟‍♀️',
      'UC727SQYUvx5pDDGQpTICNWg' => '🍂',
      'UChgTyjG-pdNvxxhdsXfHQ5Q' => '🦚',
      # HololiveEN
      'UCL_qhgtOy0dy1Agp8vkySQg' => '💀',
      'UCHsx4Hqa-1ORjQTh9TYDhww' => '🐔',
      'UCMwGHR0BTZuLsmjY_NT5Pwg' => '🐙',
      'UCoSrY_IQQVpmIRZ9Xf-y93g' => '🔱',
      'UCyl1z3jo3XHR1riLFKG5UAg' => '🔎',
      'UC8rcEBzJSleTkf_-agPM20g' => '💎',
      'UCsUj0dszADCGbF3gNrQEuSQ' => '🪐',
      'UC3n5uGu18FoCy23ggWWp8tA' => '🪶',
      'UCmbs8T6MWqUHP1tIQvSgKrg' => '⏳',
      'UCO_aKKYxn4tvrqPjcTzZ6EQ' => '🌿',
      'UCgmPnx-EEeOrZSg5Tiw7ZRQ' => '🎲'
    }

    channel_emoji.fetch(yt_channel_id, '')
  end

  private

  def print_live
    puts "Live now: #{@videos_live&.length}"
    puts '---'

    if @videos_live&.length&.positive?
      @videos_live.map(&:to_entry_str).each { |v| puts v }
    else
      puts 'No one streaming'
    end
    puts '---'
  end

  def print_upcoming
    puts "Upcoming: #{@videos_upcoming&.length}"

    if @videos_upcoming&.length&.positive?
      @videos_upcoming.map { |v| "-- #{v.to_entry_str}" }
                      .reject { |entry| entry.match?(/free/i) }
                      .each { |e| puts e }
    else
      puts 'No upcoming streams'
    end
    puts '---'
  end
end

hololive = Hololive.new
hololive.print_data
