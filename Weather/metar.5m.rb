#!/usr/bin/env ruby

# <bitbar.title>Metar</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Michael Heijmans</bitbar.author>
# <bitbar.author.github>parabuzzle</bitbar.author.github>
# <bitbar.desc>Displays the current METAR aviation weather information for the airport of your choice</bitbar.desc>
# <bitbar.image>http://github.com/parabuzzle/bitbar_metar/raw/master/metar-ifr.png</bitbar.image>
# <bitbar.dependencies>ruby</bitbar.dependencies>
# <bitbar.abouturl>http://www.github.com/parabuzzle/bitbar_metar</bitbar.abouturl>

# Handle RVM Machines
unless ENV['USING_RVM']
  # Re-run this script with RVM's default Ruby, after setting up the RVM path,
  # and setting USING_RVM to true, so that this sentry code won't run the second
  # time through.
  system(
    <<-EOF
      export USING_RVM=true
      export PATH="~/.rvm/bin:$PATH"
      rvm-auto-ruby #{File.expand_path(__FILE__)}
    EOF
  )
  # Return the exit code from running the script with RVM:
  exit $?.exitstatus.to_i
end


require 'bitbar'
require 'Base64'
require 'metar'
require 'inifile'

def flight_rules
  c = ceiling
  v = meters_to_miles(visibility)
  return "LIFR" if c <= 500  || v <= 1
  return "IFR"  if c <= 1000 || v < 3
  return "MVFR" if c <= 3000 || v < 5
  return "VFR"
end

def ceiling
  conditions = PARSED_METAR.sky_conditions
  c = 300000
  conditions.each do |condition|
    if ["broken", "overcast"].include? condition.quantity
      f = meters_to_feet(condition.height.value).to_i
      c = f if f < c
    end
  end
  return c
end

def airport_code
  config = IniFile.load(ENV['HOME'] + '/.bitbarrc')['metar']
  config['airport'].upcase
end

def visibility
  PARSED_METAR.visibility.distance.value.to_i
end

def meters_to_feet(num)
  num * 3.28084
end

def meters_to_miles(num)
  (num * 0.000621371).round(1)
end

def celius_to_fahrenheit(num)
  ((num * 1.8) + 32).round(1)
end

def format_number(num)
  num.to_s.reverse.gsub(/(\d{3})(?=\d)/, '\\1,').reverse
end

def color(fr)
  case fr
  when "LIFR"
    return "#ff00ff"
  when "IFR"
    return :red
  when "MVFR"
    return :blue
  else
    return :green
  end
end

def temperature
  PARSED_METAR.temperature_and_dew_point.temperature.value.to_i if PARSED_METAR.temperature_and_dew_point.temperature
end

def dew_point
  PARSED_METAR.temperature_and_dew_point.dew_point.value.to_i if PARSED_METAR.temperature_and_dew_point.dew_point
end

# String generation methods

def wind_gust_string
  wind = PARSED_METAR.wind
  " (Gusting at #{wind.gusts.value.round * 2})" if wind.gusts
end

def wind_string
  wind = PARSED_METAR.wind
  return unless wind
  if wind.direction == :variable_direction
    direction = "Wind: Variable Direction @ #{parsed.wind.speed.value.round * 2} Knots"
  else
    direction = "Wind #{wind.direction.value.round}° @ #{wind.speed.value.round * 2} Knots"
  end
  return "#{direction}#{wind_gust_string}"
end

def metar_issued_at_string
  "Metar Issued: #{PARSED_METAR.time.localtime.strftime('%m/%d/%Y %I:%M %p')}"
end

def visibility_string
  "Visibility #{meters_to_miles(visibility)} sm" if visibility
end


def temp_string
  "Temp: #{celius_to_fahrenheit(temperature)}° F" if temperature
end

def dew_point_string
  "Dew Point: #{celius_to_fahrenheit(dew_point)}° F" if dew_point
end

# setup variables
PARSED_METAR = Metar::Station.find_by_cccc(airport_code).parser

flight_rule = flight_rules

# Make the menu
BitBar::Menu.new do
  item flight_rule,  color: color(flight_rule)
  item airport_code, color: color(flight_rule)

  separator

  item metar_issued_at_string, color: :black
  item visibility_string, color: :black
  item temp_string, color: :black
  item dew_point_string, color: :black
  item wind_string, color: :black
  item "Sky Conditions" do
    PARSED_METAR.sky_conditions.each do |condition|
      if condition.height
        item "#{condition.quantity} - #{format_number(meters_to_feet(condition.height.value).to_i)} ft", color: :black
      else
        item "Sky Clear", color: :blue
      end
    end
  end
  separator
end


