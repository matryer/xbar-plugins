#!/usr/bin/ruby
# <bitbar.title>PagerDuty</bitbar.title>
# <bitbar.version>v2.0</bitbar.version>
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
$token    = ""
$mail     = ""
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
                opt.on("-v", "--verbose" )                                 { $verbose = true }
            end.parse!

            case $command
            when "GET"
                list_incidents
            when "ACKOWLEDGE"
                update_incident($id, "acknowledged")
            when "RESOLVE"
                update_incident($id, "resolved")
            end

        rescue StandardError => ex
            puts "ERR|color=purple"
            puts "---"
            puts ex.class
            puts ex.message
        end

    end

    def list_incidents
        out = HTTParty.get("https://api.pagerduty.com/incidents",
                           timeout: 25,
                           query:   { "since" => (Time.now-24*60*60).strftime("%Y-%m-%dT%H:%M:%S"),
                                      "sort_by" => "created_at:desc",
									  "teams" => $team_ids },
                           headers: { "Content-type"  => "application/json",
                                      "Authorization" => "Token token=#{$token}",
                                      "Accept"        => "application/vnd.pagerduty+json;version=2" })

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

                desc = incident['summary']
				desc.gsub!(/\n/,"")

                bash = "bash=#{File.expand_path(__FILE__)} param1=#{option} param2=#{incident['id']}" unless incident['status'].eql?("resolved")
                time = Time.parse(incident['created_at']).localtime.strftime("%H:%M:%S")
                puts "#{count}#{urgency} [#{time}] #{incident['incident_key']}#{urgency}|color=#{color} #{bash} refresh=true terminal=false length=100"

                begin
    				resp = JSON.parse(fetch(incident['first_trigger_log_entry']['self']))
                    client_url = resp['log_entry']['channel']['client_url']
				    client_url.gsub!(" ","%20")
                rescue
				    client_url = incident['service']['html_url']
				    client_url.gsub!(" ","%20")
                end

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

    def fetch(url)
		log("--------------------------------------")
		log("fetch: #{url}")
        out = HTTParty.get(url, headers: { "Content-type"  => "application/json",
                                           "Authorization" => "Token token=#{$token}",
                                           "Accept"        => "application/vnd.pagerduty+json;version=2" })

        log("output: #{out.body}")
        return out.body
    end

    def update_incident(id, cmd)
        body = { incident: { type: "incident_reference", status: cmd } }
        out = HTTParty.put("https://api.pagerduty.com/incidents/#{id}",
                           body:  body.to_json,
                           headers: { "Content-type"  => "application/json",
                                      "Authorization" => "Token token=#{$token}",
                                      "Accept"        => "application/vnd.pagerduty+json;version=2",
                                      "From"          => $mail.to_str })

        log("output: #{out}")
        return out
    end

    def log(line)
        return unless $verbose
        puts line
    end

    self

end.new.main
