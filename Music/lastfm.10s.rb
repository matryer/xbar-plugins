#!/usr/bin/env ruby

# Get current or last played song from Last FM
# by Eric Stiens
#
# Shows the last song played on Last.fm
# (or scrobbled from another service)
# Includes link to track info page in the dropdown

# <xbar.title>Last.fm Now Playing</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Eric Stiens</xbar.author>
# <xbar.author.github>estiens</xbar.author.github>
# <xbar.desc>Displays currently playing song from Last.fm (or scrobbles)</xbar.desc>
# <xbar.image>https://s3.amazonaws.com/f.cl.ly/items/400E102E353y3U1x2r2U/Screen%20Shot%202016-02-09%20at%2012.19.47%20PM.png?v=36addc99</xbar.image>
# <xbar.dependencies>ruby, xml-simple</xbar.dependencies>

require 'open-uri'
require 'xmlsimple'

USERNAME = '' #lastfm username
API_KEY = '' #lastfm API key

class LastfmPlugin
  def initialize(username, api_key)
    @username = username
    @api_key = api_key
    @output = nil
  end

  def call_api
    begin
      url = 'http://ws.audioscrobbler.com' + "/2.0/?method=user.getrecenttracks&user=#{@username}&api_key=#{@api_key}&limit=1"
      response = open(url).read
      XmlSimple.xml_in(response, { 'ForceArray' => false })
    rescue
      @output = "API error"
    end
  end

  def build_output
    begin
      response = call_api
      song = response['recenttracks']['track'][0]
      output_array = get_details_of_song(song)
      @output = output_array.join("\n")
    rescue
      @output = "API error"
    end
  end

  def get_details_of_song(song)
    if song['nowplaying'] == 'true'
      icon = '▶'
      playing_string = 'Now Playing'
    else
      icon='❚❚'
      playing_string = 'Last Played'
    end
    track = song['name']
    artist = song['artist']['content']
    album = song['album']['content']
    output_array = []
    output_array << "#{icon} #{track} | length=20"
    output_array << '---'
    output_array << "#{playing_string} | color = #333"
    output_array << "Track: #{track} | href = #{song['url']} color=#000080"
    output_array << "Album: #{album} | color = #333"
    output_array << "Artist: #{artist} | color = #333"
    output_array
  end

  def execute
    build_output
    puts @output
  end
end

plugin = LastfmPlugin.new(USERNAME, API_KEY)
plugin.execute
