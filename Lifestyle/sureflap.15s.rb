#!/usr/bin/env ruby

# <xbar.title>SureFlap Pet Status</xbar.title>
# <xbar.version>v1.4.0</xbar.version>
# <xbar.author>Henrik Nyh</xbar.author>
# <xbar.author.github>henrik</xbar.author.github>
# <xbar.desc>Show inside/outside status of pets using a SureFlap smart cat flap or pet door. Can also show notifications.</xbar.desc>
# <xbar.dependencies>ruby</xbar.dependencies>
#
# <xbar.var>string(VAR_EMAIL=""): App login email.</xbar.var>
# <xbar.var>string(VAR_PASSWORD=""): App login password.</xbar.var>
# <xbar.var>boolean(VAR_NOTIFICATIONS=true): Show a notification when in/out state changes?</xbar.var>
# <xbar.var>string(VAR_PER_PET_SETTINGS=""): As JSON. Missing pet names and missing values will default. E.g.: {"My Outdoor Cat": {"in": "🏠🐈", "out": "🌳🐈"}, "My Indoor Cat": {"menu_bar": false}, "My Fake Cat": {"hidden": true}} in = Custom display in menu bar when in. out = Ditto when out. menu_bar = Set false to hide in menu bar but still show in expanded menu. hidden = Set true to hide in expanded menu, too.</xbar.var>
# <xbar.var>number(VAR_CACHE_VERSION=1): Increase to clear cache if the set of pets or doors changes.</xbar.var>

# By Henrik Nyh <https://henrik.nyh.se> 2019-12-16 under the MIT license.
# Heavily based on the https://github.com/alextoft/sureflap PHP code by Alex Toft.
#
# Has no dependencies outside the Ruby standard library (uses Net::HTTP directly and painfully).

require "net/http"
require "json"
require "pp"
require "time"
require "fileutils"
require "digest"

EMAIL = ENV["VAR_EMAIL"] == "" ? nil : ENV["VAR_EMAIL"]
PASSWORD = ENV["VAR_PASSWORD"] == "" ? nil : ENV["VAR_PASSWORD"]
NOTIFICATIONS = (ENV["VAR_NOTIFICATIONS"] == "true")
CACHE_VERSION = ENV["CACHE_VERSION"]

begin
  PER_PET_SETTINGS = JSON.parse(ENV["VAR_PER_PET_SETTINGS"] || "{}")
rescue JSON::ParserError => e
  puts "🙀 Bad settings"
  puts "---"
  puts "The per-pet settings JSON is invalid:"
  puts e
  exit
end

HIDE_PETS_IN_MENU_BAR = PER_PET_SETTINGS.select { |_k, v| v["menu_bar"] == false }.keys
IGNORE_PETS_ENTIRELY = PER_PET_SETTINGS.select { |_k, v| v["hidden"] == true }.keys

ENDPOINT = "https://app.api.surehub.io"
TOKEN_PATH = File.expand_path("~/.sureflap_token")

unless EMAIL && PASSWORD
  puts "🙀 Auth missing"
  puts "---"
  puts "Please configure email and password in the plugin browser."
  exit
end

AUTH_DATA = { email_address: EMAIL, password: PASSWORD, device_id: "0" }

# From https://github.com/barsoom/net_http_timeout_errors/blob/master/lib/net_http_timeout_errors.rb
NETWORK_ERRORS = [
  EOFError,
  Errno::ECONNREFUSED,
  Errno::ECONNRESET,
  Errno::EHOSTUNREACH,
  Errno::EINVAL,
  Errno::ENETUNREACH,
  Errno::EPIPE,
  Errno::ETIMEDOUT,
  Net::HTTPBadResponse,
  Net::HTTPHeaderSyntaxError,
  Net::ProtocolError,
  Net::ReadTimeout,
  SocketError,
  Timeout::Error,  # Also covers subclasses like Net::OpenTimeout.
]

class StaleTokenError < StandardError; end

def handle_network_errors
  yield
rescue *NETWORK_ERRORS => e
  puts "🙀 Network error"
  puts "---"
  puts "Network error when trying to communicate with the SureFlap API!"
  puts "Check that you're not offline."
  puts "---"
  puts "Technical details:"
  puts "#{e.class.name}: #{e.message}"
  exit
end

def handle_non_success(response)
  return if response.code == "200"

  puts "🙀 Bad response (#{response.code})"
  puts "---"
  puts response.message
  puts response.body
  exit
end

def post(path, data)
  handle_network_errors do
    uri = URI.join(ENDPOINT, path)
    req = Net::HTTP::Post.new(uri, "Content-Type" => "application/json")
    req.body = data.to_json

    res = Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) { |http| http.request(req) }
    handle_non_success(res)

    JSON.parse(res.body)
  end
end

def get(path, token:, cache:)
  if cache
    cache_file = "/tmp/sureflap_#{Digest::SHA256.hexdigest("#{path}-#{token}")}_v#{CACHE_VERSION}"
    return JSON.parse(File.read(cache_file)) if File.exist?(cache_file)
  end

  handle_network_errors do
    uri = URI.join(ENDPOINT, path)
    req = Net::HTTP::Get.new(uri,
      "Content-Type" => "application/json",
      "Authorization" => "Bearer #{token}",
    )

    res = Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) { |http| http.request(req) }
    raw_json = res.body
    hash = JSON.parse(raw_json)

    if res.code == "401" && hash.dig("error", "message") == [ "Token Signature could not be verified." ]
      error_message = "HTTP error!\n#{res.code} #{res.message}\n#{hash.pretty_inspect}"
      raise StaleTokenError, error_message
    end

    handle_non_success(res)

    File.write(cache_file, raw_json) if cache
    hash
  end
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
  household_id = get("/api/household", token: token, cache: true).dig("data", 0, "id")

  data =
    get("/api/household/#{household_id}/pet", token: token, cache: true).fetch("data").map { |pet_data|
      id = pet_data.fetch("id")
      name = pet_data.fetch("name")
      next if IGNORE_PETS_ENTIRELY.include?(name)

      position_data = get("/api/pet/#{id}/position", token: token, cache: false).fetch("data")

      is_inside = (position_data.fetch("where") == 1)
      since = Time.parse(position_data.fetch("since")).localtime  # Convert from UTC to local time.

      [ name, [ id, is_inside, since ] ]
    }.compact.to_h

  pets_in_summary = data.keys - HIDE_PETS_IN_MENU_BAR - IGNORE_PETS_ENTIRELY
  raise "There are no pets to summarize!" if pets_in_summary.empty?

  icon = ->(is_inside) { is_inside ? "🏠" : "🌳" }

  puts pets_in_summary.map { |name|
    _id, is_inside, _since = data.fetch(name)

    custom_display = PER_PET_SETTINGS.dig(name, is_inside ? "in" : "out")
    custom_display || "#{icon.(is_inside)} #{display_name}"
  }.join("  ")

  puts "---"

  today = Date.today
  data.each do |name, (id, is_inside, since)|
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
