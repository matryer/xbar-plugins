#!/usr/bin/env ruby

# <bitbar.title>SureFlap Pet Status</bitbar.title>
# <bitbar.version>v1.2.0</bitbar.version>
# <bitbar.author>Henrik Nyh</bitbar.author>
# <bitbar.author.github>henrik</bitbar.author.github>
# <bitbar.desc>Show inside/outside status of pets using a SureFlap smart cat flap or pet door. Can also show notifications.</bitbar.desc>
# <bitbar.dependencies>ruby</bitbar.dependencies>

# By Henrik Nyh <https://henrik.nyh.se> 2019-12-16 under the MIT license.
# Heavily based on the https://github.com/alextoft/sureflap PHP code by Alex Toft.
#
# Has no dependencies outside the Ruby standard library (uses Net::HTTP directly and painfully).

# NOTE: You can configure these if you like:

# You can exclude e.g. indoor-only pets from the menu bar by listing their names here. (But all pets show if you click the menu bar item.)
# Example: [ "Foocat", "Bardog" ]
HIDE_PETS_IN_MENU_BAR = [ ]

# You can ignore pets entirely by listing their names here. They won't be listed anywhere, and no notifications sent.
# Example: [ "Foocat", "Bardog" ]
IGNORE_PETS_ENTIRELY = [ ]

# Show a notification when in/out state changes?
NOTIFICATIONS = true

# End of configuration.

require "net/http"
require "json"
require "pp"
require "time"
require "fileutils"

ENDPOINT = "https://app.api.surehub.io"
TOKEN_PATH = File.expand_path("~/.sureflap_token")
AUTH_PATH = File.expand_path("~/.sureflap_auth")

unless File.exist?(AUTH_PATH)
  puts ":warning: Run: echo \"me@example.com / my_pw\" > ~/.sureflap_auth"
  exit
end

EMAIL, PASSWORD = File.read(AUTH_PATH).strip.split(" / ")
AUTH_DATA = { email_address: EMAIL, password: PASSWORD, device_id: "0" }

class StaleTokenError < StandardError; end

def post(path, data)
  uri = URI.join(ENDPOINT, path)
  req = Net::HTTP::Post.new(uri, "Content-Type" => "application/json")
  req.body = data.to_json

  res = Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) { |http| http.request(req) }
  hash = JSON.parse(res.body)

  raise "HTTP error!\n#{res.code} #{res.message}\n#{hash.pretty_inspect}" unless res.code == "200"

  hash
end

def get(path, token:)
  uri = URI.join(ENDPOINT, path)
  req = Net::HTTP::Get.new(uri,
    "Content-Type" => "application/json",
    "Authorization" => "Bearer #{token}",
  )

  res = Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) { |http| http.request(req) }
  hash = JSON.parse(res.body)

  error_message = "HTTP error!\n#{res.code} #{res.message}\n#{hash.pretty_inspect}" unless res.code == "200"

  if res.code == "401" && hash.dig("error", "message") == [ "Token Signature could not be verified." ]
    raise StaleTokenError, error_message
  elsif res.code != "200"
    raise error_message
  end

  hash
end

def refresh_token
  token = post("/api/auth/login", AUTH_DATA).dig("data", "token")
  File.write(TOKEN_PATH, token)
  token
end

# This method reuses an existing token until it becomes stale.
def with_fresh_token
  retried = false

  begin
    token = File.exist?(TOKEN_PATH) && File.read(TOKEN_PATH) || refresh_token()
    yield(token)
  rescue StaleTokenError
    raise if retried  # Avoid endless loops.
    retried = true

    FileUtils.rm(TOKEN_PATH)
    retry
  end
end

with_fresh_token do |token|
  # We assume a single household.
  household_id = get("/api/household", token: token).dig("data", 0, "id")

  data =
    get("/api/household/#{household_id}/pet", token: token).fetch("data").map { |pet_data|
      id = pet_data.fetch("id")
      position_data = get("/api/pet/#{id}/position", token: token).fetch("data")

      name = pet_data.fetch("name")
      is_inside = (position_data.fetch("where") == 1)
      since = Time.parse(position_data.fetch("since"))

      [ name, [ id, is_inside, since ] ]
    }.to_h

  pets_in_summary = data.keys - HIDE_PETS_IN_MENU_BAR - IGNORE_PETS_ENTIRELY
  raise "There are no pets to summarize!" if pets_in_summary.empty?

  icon = ->(is_inside) { is_inside ? "🏠" : "🌳" }

  puts pets_in_summary.map { |name|
    _id, is_inside, _since = data.fetch(name)
    "#{icon.(is_inside)} #{name}"
  }.join("  ")

  puts "---"

  today = Date.today
  data.each do |name, (id, is_inside, since)|
    next if IGNORE_PETS_ENTIRELY.include?(name)

    if NOTIFICATIONS
      inside_state_path = "/tmp/sureflap_#{id}_is_inside"
      previous_is_inside_string = File.read(inside_state_path) rescue nil

      if previous_is_inside_string && previous_is_inside_string != is_inside.to_s
        system("osascript", "-e", %{display notification "#{icon.(is_inside)} #{name} #{is_inside ? "has entered… Hi #{name}!" : "has left… Bye #{name}!" }" with title "Cat flap"})
      end

      File.write(inside_state_path, is_inside)
    end

    formatting_string =
      case since.to_date
      when today then "%H:%M"
      when today - 1 then "yesterday at %H:%M"
      when (today - 2)..(today - 6) then "%a at %H:%M"
      else "%b %-d %Y at %H:%M"
      end
    puts "#{icon.(is_inside)} #{name} is #{is_inside ? "inside" : "outside"} since #{since.strftime(formatting_string)}."
  end
end
