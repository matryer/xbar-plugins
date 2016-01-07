#!/usr/bin/env ruby

require 'wunderground'

# Weather Underground plugin for BitBar
# Adam Snodgrass <asnodgrass@sarchasm.us>
# 
# Requires 'wunderground' gem and an API key from WU.
# http://www.wunderground.com/weather/api
#
# usage: Wunderground.1h.rb <api-key> [location-string [imperial-units]]
# Location-string can be any valid location identifier per WU's API docs.
# Imperial-units, if set to any value, will return imperial rather than SI.
#
# Alternatively, you can modify the values below to avoid using a wrapper
# script.

API_KEY = nil
LOCATION = nil
IMPERIAL = nil

class WeatherPlugin
  def initialize(apikey, location = 'autoip', imperial = false)
    @wxu = Wunderground.new(apikey)
    @loc = location
    @imperial = imperial
  end

  def output
    cond = current_conditions
    return if cond.nil?
    update = formatted_time(cond['observation_epoch'].to_i)
    puts header(cond)
    puts '---'
    puts "Conditions: #{cond['weather']}"
    puts "Feels Like: #{temperature(cond, 'feelslike')}"
    puts "Dewpoint: #{temperature(cond, 'dewpoint')}"
    if cond['temp_c'].to_i <= 10
      puts "Wind Chill: #{temperature(cond, 'windchill')}"
    elsif cond['temp_c'].to_i >= 27
      puts "Heat Index: #{temperature(cond, 'heat_index')}"
    end
    puts "Relative Humidity: #{cond['relative_humidity']}"
    puts "Pressure: #{pressure(cond)}"
    puts "Visibility: #{visibility(cond)}"
    puts "Winds: #{winds(cond)}"
    puts "Location: #{cond['observation_location']['full']}"
    puts "Station ID: #{cond['station_id']}"
    puts "Station Report: #{update}"
  end

  private

  def current_conditions
    if @loc.downcase.eql?('autoip')
      autoloc = @wxu.geolookup_for('autoip')
      @loc = format('%s,%s',
        autoloc['location']['lat'], autoloc['location']['lon']
      )
    end
    data = @wxu.conditions_for(@loc)
    if data['response'].key?('error')
      puts data['response']['error']['type']
      return nil
    end
    data['current_observation']
  end

  def header(cond)
    "#{icon(cond)} #{temperature(cond, 'temp')}"
  end

  def temperature(cond, name)
    abbrev = @imperial ? 'F' : 'C'
    var = @imperial ? "#{name}_f" : "#{name}_c"
    format('%sº %s', cond[var], abbrev)
  end

  def pressure(cond)
    abbrev = @imperial ? 'in/hg' : 'kPa'
    pressure = @imperial ?  cond['pressure_in'] : pressure_to_kpa(cond)
    format('%s %s, %s', pressure, abbrev, pressure_trend(cond))
  end

  def visibility(cond)
    abbrev = @imperial ? 'mi' : 'km'
    var = @imperial ? 'visibility_mi' : 'visibility_km'
    format('%s %s', cond[var], abbrev)
  end

  def winds(cond)
    abbrev = @imperial ? 'mph' : 'kph'

    if cond["wind_#{abbrev}"].to_f == 0
      winds = 'Calm'
    else
      winds = format('%s @ %s %s',
        cond['wind_dir'], cond["wind_#{abbrev}"], abbrev
      )
    end

    if cond["wind_gust_#{abbrev}"].to_f > 0
      winds << format(', gusting to %s %s', cond["wind_gust_#{abbrev}"], abbrev)
    end

    winds
  end

  def pressure_to_kpa(cond)
    cond['pressure_mb'].to_f / 10
  end

  def pressure_trend(cond)
    case cond['pressure_trend']
    when '+' then 'rising'
    when '-' then 'falling'
    when '0' then 'steady'
    end
  end

  def formatted_time(epoch)
    Time.at(epoch).strftime('%a, %d %b %Y %T %z')
  end

  def icon(cond)
    case File.basename(cond['icon_url'].downcase, '.gif')
    when 'chanceflurries', 'chancesnow', 'flurries', 'snow',
        'chancesleet', 'sleet', 'nt_chanceflurries', 'nt_chancesnow',
        'nt_flurries', 'nt_snow', 'nt_chancesleet', 'nt_sleet'
      '❄️'
    when 'chancerain', 'rain', 'nt_chancerain', 'nt_rain'
      '☔️'
    when 'chancetstorms', 'tstorms', 'nt_chancetstorms', 'nt_tstorms'
      '⚡️'
    when 'nt_clear', 'nt_mostlysunny', 'nt_sunny'
      '🌙'
    when 'clear', 'mostlysunny', 'sunny'
      '☀️'
    when 'cloudy', 'mostlycloudy', 'fog', 'hazy', 'nt_cloudy',
        'nt_mostlycloudy', 'nt_fog', 'nt_hazy'
      '☁️'
    when 'partlycloudy', 'partlysunny', 'nt_partlycloudy', 'nt_partlysunny'
      '⛅️'
    end
  end
end

if ARGV.empty?
  ARGV << API_KEY
  ARGV << LOCATION
  ARGV << IMPERIAL
end
plugin = WeatherPlugin.new(*ARGV)
plugin.output
