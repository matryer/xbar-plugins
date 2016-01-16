#!/usr/bin/ruby
# <bitbar.title>PagerDuty</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.a uthor>Allan Frese</bitbar.author>
# <bitbar.author.github>frese</bitbar.author.github>
# <bitbar.desc>Shows current pagerduty alert status.</bitbar.desc>
# <bitbar.dependencies>ruby (httparty gem)</bitbar.dependencies>

require "json"
require "pp"
require "optparse"
require "httparty"

# Setup PagerDuty domain and user token
TOKEN=""
DOMAIN=""
USERID=""

class PagerDuty

    def main

        # Init some defaults
        $verbose = false
        $id      = nil
        $command = "GET"

        OptionParser.new do |opt|
            opt.banner = "Usage: #{$0} [options]"
            opt.on("-a", "--ackowledge ID", "Acknowledge an incident") { |id| $command = "ACKOWLEDGE"; $id = id }
            opt.on("-r", "--resolve ID", "Resolve an incident")        { |id| $command = "RESOLVE";    $id = id }
            opt.on("-u", "--users", "list users along with their id")  { $command = "users" }
            opt.on("-v", "--verbose" )                                 { $verbose = true }
        end.parse!

        case $command
        when "GET"
            list_incidents
        when "ACKOWLEDGE"
            update_incident($id, "acknowledged")
        when "RESOLVE"
            update_incident($id, "resolved")
        when "REASSIGN"
        when "SNOOZE"
        when "users"
            out = HTTParty.get("https://#{DOMAIN}.pagerduty.com/api/v1/users",
                               headers: {"Content-type" => "application/json", "Authorization" => "Token token=#{TOKEN}"})

            puts "Raw output: #{out}" if $verbose
            usr = JSON.parse(out.body)
            usr['users'].each { |u|
                puts "id: #{u['id']} - name: #{u['name']}"
            }
        end

    end

    def list_incidents
        begin
            out = HTTParty.get("https://#{DOMAIN}.pagerduty.com/api/v1/incidents",
                               query:   { "status" => "triggered,acknowledged" },
                               headers: { "Content-type" => "application/json", "Authorization" => "Token token=#{TOKEN}"})

            puts "Raw output: #{out}" if $verbose
            pd = JSON.parse(out.body)

            if pd['incidents'].empty?
                puts "OK|color=green"
            else
                s = pd['incidents'].count==1 ? "" : "s"
                puts "#{pd['incidents'].count} Alert#{s}|color=red"
                puts "---"
                pd['incidents'].each { |incident|
                    puts "-----------------------" if $verbose
                    pp incident if $verbose
                    color  = incident['urgency'].eql?("high") ? "red" : "yellow"
                    status = incident['status'] #.eql?("triggered") ? "TRG" : "ACK"
                    option = incident['status'].eql?("triggered") ? "-a" : "-r"

                    desc = "No description"
                    desc = incident['trigger_summary_data']['subject'] if incident['trigger_summary_data'].include?('subject')
                    desc = incident['trigger_summary_data']['description'] if incident['trigger_summary_data'].include?('description')
                    desc = incident['incident_key'] if incident['incident_key'].length > 10

                    bash = "#{File.expand_path(__FILE__)} param1=#{option} param2=#{incident['id']}"
                    puts "[#{incident['incident_number']}] #{desc} [#{status}]|color=#{color} bash=#{bash} terminal=false length=100"
                }
            end
        rescue Exception => ex
            puts "ERR|color=purple"
            puts "---"
            puts ex.class
            puts ex.message
            puts pd unless pd.nil?
        end
    end

    def update_incident(id, cmd)
        body = { requester_id: USERID, incidents: [{ id: id, status: cmd }] }
        out = HTTParty.put("https://#{DOMAIN}.pagerduty.com/api/v1/incidents",
                           body:  body.to_json,
                           headers: { "Content-type" => "application/json", "Authorization" => "Token token=#{TOKEN}"})

        puts "Raw output: #{out}" if $verbose
        return out
    end

    self

end.new.main
