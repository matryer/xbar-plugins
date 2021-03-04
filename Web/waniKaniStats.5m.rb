#!/usr/bin/env ruby
# frozen_string_literal: true

# <bitbar.title>WaniKani Stats</bitbar.title>
# <bitbar.version>v0.2.0</bitbar.version>
# <bitbar.author>Nzebo</bitbar.author>
# <bitbar.author.github>Nzebo</bitbar.author.github>
# <bitbar.desc>Displays user details & review + lesson tracking for WaniKani.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/orRd3Ga.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>

require 'net/http'
require 'json'
require 'time'
require 'date'


# WaniKani API Token v2 - Retrieved from https://www.wanikani.com/settings/personal_access_tokens
API_TOKEN = ''

COLORS = {
  Radicals: '#00AAFF',
  Kanji: '#FF00AA',
  Vocabulary: '#7c04b7'
}.freeze


##
# Base64 encoded icon for the menu
IMG_BASE64 = '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' \
'3z4SPvL0Q+aHsdH8j5ofD31y+dT5Oe7zi7HlX3BfSr+afW39FvLt8Xjm+LiYI+FMtgIoROGUFADeHg' \
'SAkgAA7SYAxAVTPfWkQFP/AyYJ/Cee6rsnxQmAmjYAYtcDEOkNwD5EmQhTkTEc0WhvANvbK/SfIk2x' \
't5uKRWpGWpOS8fF3SP+IMwPga8/4+Fjz+PjXGqTYhwC0fZjq5SeEehgA7z1Odq5RN4aSh8G/yD8AZG' \
'YR7evMlgYAAAAJcEhZcwAAFiUAABYlAUlSJPAAAAILaVRYdFhNTDpjb20uYWRvYmUueG1wAAAAAAA8' \
'eDp4bXBtZXRhIHhtbG5zOng9ImFkb2JlOm5zOm1ldGEvIiB4OnhtcHRrPSJYTVAgQ29yZSA1LjQuMC' \
'I+CiAgIDxyZGY6UkRGIHhtbG5zOnJkZj0iaHR0cDovL3d3dy53My5vcmcvMTk5OS8wMi8yMi1yZGYt' \
'c3ludGF4LW5zIyI+CiAgICAgIDxyZGY6RGVzY3JpcHRpb24gcmRmOmFib3V0PSIiCiAgICAgICAgIC' \
'AgIHhtbG5zOnRpZmY9Imh0dHA6Ly9ucy5hZG9iZS5jb20vdGlmZi8xLjAvIj4KICAgICAgICAgPHRp' \
'ZmY6UmVzb2x1dGlvblVuaXQ+MjwvdGlmZjpSZXNvbHV0aW9uVW5pdD4KICAgICAgICAgPHRpZmY6Q2' \
'9tcHJlc3Npb24+MTwvdGlmZjpDb21wcmVzc2lvbj4KICAgICAgICAgPHRpZmY6T3JpZW50YXRpb24+' \
'MTwvdGlmZjpPcmllbnRhdGlvbj4KICAgICAgICAgPHRpZmY6UGhvdG9tZXRyaWNJbnRlcnByZXRhdG' \
'lvbj4yPC90aWZmOlBob3RvbWV0cmljSW50ZXJwcmV0YXRpb24+CiAgICAgIDwvcmRmOkRlc2NyaXB0' \
'aW9uPgogICA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgoPRSqTAAAHDklEQVRYCcVYa2hcRRSeu7t5SU' \
'NKWwpBLRWLKCLaWkSq/SH6o9o/LaKIaKv+UBGRomiFKoL4QxELoj8MUqUIlaY+QGixVo21Qdoa64PE' \
'xBKaamvSxGx3s5ve973H78zj3rvbrKRp1AlnZ+6Z8/jumZkz50YI3YgoZ8bcUzflKaA14O+kMHxE8o' \
'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'


# class holding all of the user's WaniKani information
class WaniKani
  def initialize(api_token)
    @api_token = api_token
    @reviews = wanikani_api('https://api.wanikani.com/v2/assignments?immediately_available_for_review')
    @lessons = wanikani_api('https://api.wanikani.com/v2/assignments?immediately_available_for_lessons')
    @user = wanikani_api('https://api.wanikani.com/v2/user')
    @summary = wanikani_api('https://api.wanikani.com/v2/summary')
  end

  # method for making API calls to WaniKani
  def wanikani_api(resource)
    uri = URI.parse(resource)
    http = Net::HTTP.new(uri.host, uri.port)
    http.use_ssl = true
    request = Net::HTTP::Get.new uri.request_uri
    request.add_field('Authorization', "Token #{API_TOKEN}")
    response = http.request(request)

    JSON.parse(response.body)
  end

  # calculate the next time a review is available
  def calc_next_review_time
    next_review_time = Time.parse(@summary['data']['next_reviews_at']).localtime

    if next_review_time < Time.now.localtime
      ' Now! | href = https://www.wanikani.com/'
    else
      next_review_time.strftime(' %l %p')
    end

  end

  # calculates the details (such as # of items per category) for assignments
  def calc_assignment_details(type)
    assignment_details = {
        Radicals: 0,
        Kanji: 0,
        Vocabulary: 0
    }

    if type == 'reviews'
      next_review_time = Time.parse(@summary['data']['next_reviews_at']) + 1
      next_review_time = next_review_time.to_datetime.rfc3339.to_s.gsub(/\+[0-9:]+/, 'Z')

      now = Time.now.utc.to_datetime.rfc3339.gsub(/\+[0-9:]+/, 'Z')

      if next_review_time > now
        assignment_data = wanikani_api("https://api.wanikani.com/v2/assignments?available_after=#{now}&available_before=#{next_review_time}")
      else
        assignment_data = @reviews
      end
    elsif type == 'lessons'
      assignment_data = @lessons
    end



    assignment_data['data'].each do |assignment|
      case assignment['data']['subject_type']
      when 'radical'
        assignment_details[:Radicals] += 1
      when 'kanji'
        assignment_details[:Kanji] += 1
      when 'vocabulary'
        assignment_details[:Vocabulary] += 1
      else
        puts "Unknown assignment type: #{assignment['data']['subject_type']}"
      end

    end

    assignment_details

  end

  # calculate how long the user has been on their current level
  def calc_level_duration

    level_data = wanikani_api('https://api.wanikani.com/v2/level_progressions')

    if level_data['data'][level_data['data'].length - 1]['data']['started_at'].nil?
      return 0
    end

    level_start_time = Time.parse(level_data['data'][level_data['data'].length - 1]['data']['started_at'])

    now = Time.now

    ((now - level_start_time) / 3600 / 24).round(2)

  end

  # calculate the percentages of each category for the current level
  def calc_level_percentage

    subject_items = []
    totals = {
      Radical: {
        total: 0,
        passed: 0,
        items: []
      },
      Kanji: {
        total: 0,
        passed: 0,
        items: []
      },
      Vocabulary: {
        total: 0,
        passed: 0,
        items: []
      }
    }

    level_subject_data = wanikani_api("https://api.wanikani.com/v2/assignments?levels=#{@user['data']['level']}")

    reset_time = Time.parse(latest_reset)

    level_subject_data['data'].each do |assignment|

      start_time = assignment['data']['started_at']

      if start_time.nil?
        totals[assignment['data']['subject_type'].capitalize.to_sym][:total] += 1
        next
      end

      next if Time.parse(start_time) < reset_time

      totals[assignment['data']['subject_type'].capitalize.to_sym][:total] += 1
      subject_items.push(
        id: assignment['data']['subject_id'],
        type: assignment['data']['subject_type'].capitalize,
        character: '',
        srs_stage: assignment['data']['srs_stage_name'],
        srs_stage_id: assignment['data']['srs_stage'],
        document_url: ''
      )

      if assignment['data']['passed']
        totals[assignment['data']['subject_type'].capitalize.to_sym][:passed] += 1
      end

    end

    totals = get_assignment_subjects(totals, subject_items)

    totals

  end

  # get subjects data using the provided subject IDs
  def get_assignment_subjects(subject_items, subject_items_array)

    subject_data = wanikani_api("https://api.wanikani.com/v2/subjects?ids=#{subject_items_array.map{|item| item[:id]}.join(',')}")


    subject_items_array.each_with_index do |item, idx|

      match = subject_data['data'].find { |subject| subject['id'] == item[:id] }

      subject_items_array[idx][:character] = match['data']['characters']
      subject_items_array[idx][:document_url] = match['data']['document_url']

      subject_items[match['object'].capitalize.to_sym][:items].push(subject_items_array[idx])

    end

    subject_items

  end

  def latest_reset

    reset_data = wanikani_api("https://api.wanikani.com/v2/resets")

    latest_reset_time = ''

    unless reset_data['data'].empty?
      latest_reset = reset_data['data'][reset_data['data'].length - 1]

      latest_reset_time = Time.parse(latest_reset['data']['created_at']).to_s

    end

    latest_reset_time
  end

  # print all of the data to stdout for bitbar
  def print_data
    if API_TOKEN == ''
      puts 'ERROR'
      puts '---'
      puts 'Please edit the plugin script and add your WaniKani API token to the API_TOKEN variable at the top'
      return
    end
    puts "L: #{@lessons['total_count']} R: #{@reviews['total_count']}"
    puts '---'
    puts "  WaniKani Stats | size=14 color=#BFBFBF trim=false image=#{IMG_BASE64}"
    puts "---"
    puts 'User Profile | size=12 color=#BFBFBF'
    puts "#{@user['data']['username']} - Level: #{@user['data']['level']}"
    puts "Level duration: #{calc_level_duration} days"
    puts '---'
    puts 'Current Level Progress | size=12 color=#BFBFBF'
    level_percentage = calc_level_percentage
    unless level_percentage == 0
      level_percentage.each do |key, val|
        puts "#{key} [#{val[:passed]}/#{val[:total]}] - #{((val[:passed].to_f / val[:total].to_f) * 100).round(0)}%"

        header = ''
        level_percentage[key.to_sym][:items].sort_by!{|obj| obj[:srs_stage_id]}.each do |item|
          if item[:srs_stage] != header
            header = item[:srs_stage]
            color = if header.include? 'Apprentice'
                      '#FF00AA'
                    elsif header.include? 'Guru'
                      '#7c04b7'
                    else
                      '#BFBFBF'
                    end
            puts "-- #{header} | size=12 color=#{color}"
          end
          if !item[:character].nil?
            puts "--  #{item[:character]} | href=#{item[:document_url]}"
          else
            puts "--  Non UTF-8 character | href=#{item[:document_url]}"
          end
        end

      end
    end
    puts '---'
    puts 'Lessons | size=12 color=#BFBFBF'
    calc_assignment_details('lessons').each do |key, val|
      puts "#{key}: #{val} | color=#{COLORS[key]}"
    end
    puts '---'
    puts 'Reviews | size=12 color=#BFBFBF'
    puts "Next reviews:#{calc_next_review_time}"
    calc_assignment_details('reviews').each do |key, val|
      puts "#{key}: #{val} | color=#{COLORS[key]}"
    end

  end
end

w = WaniKani.new(API_TOKEN)
w.print_data
