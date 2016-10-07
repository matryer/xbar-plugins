#!/usr/bin/ruby
# <bitbar.title>PagerDuty</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Allan Frese</bitbar.author>
# <bitbar.author.github>frese</bitbar.author.github>
# <bitbar.desc>Shows current pagerduty alert status.</bitbar.desc>
# <bitbar.dependencies>ruby (httparty gem)</bitbar.dependencies>
# <bitbar.image>http://i.imgur.com/5onainp.png</bitbar.image>

require "json"
require "pp"
require "optparse"
require "httparty"
require "date"

#--------------------------------------------------------------------
# Set some configuration for PagerDuty
$token  = ""
$domain = ""
$userid = ""
$team_ids = ""
#--------------------------------------------------------------------

class PagerDuty

    def main
        begin
            # Init some defaults
            $verbose = false
            $id      = nil
            $command = "GET"
            $color   = Hash.new

			if ENV['BitBarDarkMode'].nil?
				$color['normal']       = 'black'
				$color['triggered']    = 'red'
				$color['acknowledged'] = 'orange'
				$color['resolved']     = 'green'
			else
				$color['normal']       = 'white'
				$color['triggered']    = 'red'
				$color['acknowledged'] = 'yellow'
				$color['resolved']     = 'green'
			end

            OptionParser.new do |opt|
                opt.banner = "Usage: #{$0} [options]"
                opt.on("-a", "--ackowledge ID", "Acknowledge an incident") { |id| $command = "ACKOWLEDGE"; $id = id }
                opt.on("-r", "--resolve ID", "Resolve an incident")        { |id| $command = "RESOLVE";    $id = id }
                opt.on("-u", "--users", "list users along with their id")  { $command = "USERS" }
                opt.on("-v", "--verbose" )                                 { $verbose = true }
            end.parse!

            case $command
            when "GET"
                list_incidents
            when "ACKOWLEDGE"
                update_incident($id, "acknowledged")
            when "RESOLVE"
                update_incident($id, "resolved")
            when "USERS"
                out = HTTParty.get("https://#{$domain}.pagerduty.com/api/v1/users",
                                   headers: {"Content-type" => "application/json",
                                             "Authorization" => "Token token=#{$token}"})

                log("output: #{out}")
                usr = JSON.parse(out.body)
                usr['users'].each { |u|
                    puts "id: #{u['id']} - name: #{u['name']}"
                }
            end

        rescue StandardError => ex
            puts "ERR|color=purple"
            puts "---"
            puts ex.class
            puts ex.message
        end

    end

    def list_incidents
        out = HTTParty.get("https://#{$domain}.pagerduty.com/api/v1/incidents",
                           timeout: 25,
                           query:   { "since" => (Time.now-24*60*60).strftime("%Y-%m-%dT%H:%M:%S"),
                                      "sort_by" => "created_on:desc",
				      "teams" => $team_ids },
                           headers: { "Content-type" => "application/json",
                                      "Authorization" => "Token token=#{$token}"})

        pd = JSON.parse(out.body)
        incidents = Array.new
        @count = 0
        @color = "yellow"
        pd['incidents'].each { |i|
            log("Incident: #{i}")
            if i['status'].eql?("resolved")
                @inc = nil
                incidents.each { |i2| @inc = i2 if i2['incident_key'].eql?(i['incident_key']) and i2['status'].eql?("resolved") }
                if @inc.nil?
                    i['count'] = 1
                    incidents.push(i)
                else
                    @inc['count'] += 1
                end
            else
                @count += 1
                @color = "red" if i['status'].eql?("triggered")
                i['count'] = 1
                incidents.push(i)
            end
        }

        if incidents.empty?
            puts "OK|color=green"
        else
            puts @count>0 ? "#{@count} Alert#{@count==1 ? "" : "s"}|color=#{@color} dropdown=false" : "OK|color=green dropdown=false"
            puts "---"
            incidents.each { |incident|
                log(incident.inspect)
                urgency = incident['urgency'].eql?("high") ? "⚡" : ""
                status  = incident['status']
                color   = $color[status]
                option  = incident['status'].eql?("triggered") ? "-a" : "-r"
                count   = incident['count'] > 1 ? "(#{incident['count']})" : ""

                desc = "No description"
                desc = incident['trigger_summary_data']['subject'] if incident['trigger_summary_data'].include?('subject')
                desc = incident['trigger_summary_data']['description'] if incident['trigger_summary_data'].include?('description')

                bash = "bash=#{File.expand_path(__FILE__)} param1=#{option} param2=#{incident['id']}" unless incident['status'].eql?("resolved")
                time = Time.parse(incident['created_on']).localtime.strftime("%H:%M:%S")
                puts "#{count}#{urgency} [#{time}] #{incident['incident_key']}#{urgency}|color=#{color} #{bash} refresh=true terminal=false length=100"
				client_url = incident['html_url']
				client_url = incident['trigger_summary_data']['client_url'] unless incident['trigger_summary_data'].empty? or incident['trigger_summary_data']['client_url'].nil?
				client_url.gsub!(" ","%20")
				if desc.length >= 100
                    puts "#{desc[0..99]}...|color=#{$color['normal']} size=11 href=#{client_url}"
				    puts "...#{desc[100..200]}|alternate=true color=#{$color['normal']} size=11"
				else
                    puts "#{desc}...|color=#{$color['normal']} size=11 href=#{client_url}"
				end
                puts "---"
            }
        end
    end

    def update_incident(id, cmd)
        body = { requester_id: $userid, incidents: [{ id: id, status: cmd }] }
        out = HTTParty.put("https://#{$domain}.pagerduty.com/api/v1/incidents",
                           body:  body.to_json,
                           headers: { "Content-type" => "application/json",
                                      "Authorization" => "Token token=#{$token}"})

        log("output: #{out}")
        return out
    end

    def log(line)
        return unless $verbose
        puts line
    end

    self

end.new.main
