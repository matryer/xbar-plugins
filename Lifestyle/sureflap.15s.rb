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

# Increase this number when you add 😻, remove 😿 or rename pets or flaps/doors. You don't need to increase it if you just change the configuration above.
# As long as this number remains unchanged, we will assume nothing's changed, which makes things faster and means we can check status more frequently.
CACHE_VERSION = 1

# End of configuration.


require "net/http"
require "json"
require "pp"
require "time"
require "fileutils"
require "digest"

ENDPOINT = "https://app.api.surehub.io"
TOKEN_PATH = File.expand_path("~/.sureflap_token")
AUTH_PATH = File.expand_path("~/.sureflap_auth")

unless File.exist?(AUTH_PATH)
  puts ":warning: Run: echo \"me@example.com / my_pw\" > ~/.sureflap_auth"
  exit
end

EMAIL, PASSWORD = File.read(AUTH_PATH).strip.split(" / ")
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
  puts "🙀"
  puts "---"
  puts "Network error when trying to communicate with the SureFlap API!"
  puts "Check that you're not offline."
  puts "---"
  puts "Technical details:"
  puts "#{e.class.name}: #{e.message}"
  exit 1
end

def post(path, data)
  handle_network_errors do
    uri = URI.join(ENDPOINT, path)
    req = Net::HTTP::Post.new(uri, "Content-Type" => "application/json")
    req.body = data.to_json

    res = Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) { |http| http.request(req) }
    hash = JSON.parse(res.body)

    raise "HTTP error!\n#{res.code} #{res.message}\n#{hash.pretty_inspect}" unless res.code == "200"

    hash
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

    error_message = "HTTP error!\n#{res.code} #{res.message}\n#{hash.pretty_inspect}" unless res.code == "200"

    if res.code == "401" && hash.dig("error", "message") == [ "Token Signature could not be verified." ]
      raise StaleTokenError, error_message
    elsif res.code != "200"
      raise error_message
    end

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
    "#{icon.(is_inside)} #{name}"
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
