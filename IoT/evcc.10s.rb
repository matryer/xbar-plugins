#!/usr/bin/env ruby

# Metadata allows your plugin to show up in the app, and website.
#
#  <xbar.title>evcc</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>naltatis</xbar.author>
#  <xbar.author.github>naltatis</xbar.author.github>
#  <xbar.desc>shows evcc solar charging stats</xbar.desc>
#  <xbar.image>https://imgur.com/G1N1Ke0</xbar.image>
#  <xbar.dependencies>ruby</xbar.dependencies>
#  <xbar.abouturl>https://evcc.io/</xbar.abouturl>

# Variables become preferences in the app:
#
#  <xbar.var>string(VAR_SERVER="http://evcc.local:7070"): You local evcc instance.</xbar.var>

require 'net/http'
require 'json'

LIGHNING_REGULAR = 'iVBORw0KGgoAAAANSUhEUgAAAAoAAAAQCAQAAACFdibLAAAAnUlEQVR4AV3BsUpCYQAG0C9rahC5ILa1+gSN4eprBI2tQTX4Au3tLm5B0/8IQeAmDtbSEtgqSOVwQoLb1XPyz4nscWvlWBr0rFBJgwdbldT0bcxRSc2TDxf4tPTmNIlzXDpy597Uj14ceDFzKFsWHiUG2FhbuzLAUKLj2sjIq2LiXUtqiqlvN9Kg4EtXGhSMZYeCM9lh7Fn26GjLn1/w5Ye7aDKtqgAAAABJRU5ErkJggg=='
LIGHNING_BOLD = 'iVBORw0KGgoAAAANSUhEUgAAAAoAAAAQCAQAAACFdibLAAAAg0lEQVR42mP4zwCHfDAWQijx/5f/wqiC3P+f////XwlVsP7/f3RBif9fMAVn/oeA+/9v/z/5XwokpPX/z39koA4S3IwitBek3eA/KggBCTL9r/jf87/n/3Ww0LP/LMiOXw4WbEb1EUjwz39ZTMGN6H4HCXqgC076f+c/I7ogx39JGBsAyKjWI/aAmiwAAAAASUVORK5CYII='

GRID_IN = 'iVBORw0KGgoAAAANSUhEUgAAABwAAAAQCAYAAAAFzx/vAAABdElEQVR4AWIYcABonR5A/QyjOI7Ptm3bRm5Wbra9hTlrZp6tMPPatm3b+j7X9/yNX32qU+d5f72qrq6eDX8UCvHYi7bWKGmD4biKIqikwLWeP1Qq8BpT0cHcsrbYgB9Ix0uo3G62MxMq/5CMEBzHAHMKLyIeKsVwgUo0vtazh0owcqCSjSfo3OxaHXEEl9BRzg1Lb2Bu3NGtWeFRFMEZveSs+651P9JNYlXeWTl8MFXM4/WVtYaKPc7XuwWVzWJd3pk3xsjZ0DtdCV25gnZazuxGHgIwRc6Gfg9nxGE5utQbijeIQy8t59pjG8oRiRlinqyrcAuKkIcbmIfxOIQgqFxDa3lW3KkrestZLvfDZ2ThNXIQA3+UwgH+SMc4WSbu9BTay1kub0IKfuE1ilCCQlTABe9RhvvgLs1M/e0/hUoFyvAXSzAYD5CPCqhEY64lhRMQgyR8xxq0Ex/TVDxCBLJx3pLCPjiM1eho4Cuei9NYaE5XDYKZWInmDpgxAAAAAElFTkSuQmCC'
GRID_OUT = 'iVBORw0KGgoAAAANSUhEUgAAABwAAAAQCAYAAAAFzx/vAAABd0lEQVR4AWIYcABopR5ANYnCMI6vbdu2rbxWdq3tDetcayusbdu4ite2bX7633qr6TT4NPWr8x49Y6fTORFhqFCkYS0a+yOkEfriKCpRf2QjSIRJnw0PMBrNvA1rjOX4iDzck83PauaMl77vyEI0tqObN4EHkSYbViFQ2kl4J/5IXxSKpV2Em2gp+wzFXr1aDXwIb48QtJF99sOGe+io1qZXbXJLV5qsaYWHsOMKOit1S6OFDWXzP9gvzkjfKosT7YjrmisbpNQd9RbNh9FxBE1MApvjIGz4j75K3U3v8whAKuaileiNh9LfwSTsBGx4iX5K3V1v0WpUohSnMAVDsQmRcpXH0FBn7Tbtlai1XlgXvEAhHqAYyQhDDf5KOw9DdNYvwEV016v1AlciG58lsBLVqIANgXiCWtmooS+/to64pfl91eIbZqEnLqEMNs1PYbIvgcOQjEx8wGI0UV6m0biCeBRhvy+BnbAZi9Dc4ic/Gbsx3ZssF2RzUgryg2B2AAAAAElFTkSuQmCC'

PHASES = {1 => "⓵", 2 => "⓶", 3 => "⓷"}

begin
    response = JSON.parse(Net::HTTP.get(URI("#{ENV['VAR_SERVER']}/api/state")))
rescue
    puts 'server not found'
    return
end 

result = response["result"]
gridPower = result["gridPower"]
totalChargePower = (result["loadpoints"].reduce(0) { |sum, lp| sum + lp["chargePower"] }) / 1000.0

if totalChargePower > 0
    puts " %0.1f kW| trim=false | image='#{LIGHNING_BOLD}'" % [totalChargePower]
elsif gridPower < 0
    export = (gridPower * -1.0) / 1000.0
    puts " %0.1f kW| trim=false | image='#{GRID_IN}'" % [export]
else
    import = gridPower / 1000.0
    puts " %0.1f kW| trim=false | image='#{GRID_OUT}'" % [import]
end

puts "---"

# Site

pvConfigured = result["pvConfigured"]
batteryConfigured = result["batteryConfigured"]
homePower = result["homePower"] / 1000.0
gridPower = result["gridPower"] / 1000.0

home = sprintf("🏠 %0.1f kW ", homePower)
grid = gridPower < 0 ? sprintf("🏭◀ %0.1f kW", gridPower * -1.0) : sprintf("🏭▶ %0.1f kW", gridPower)
pv = ""
if pvConfigured
    pvPower = result["pvPower"] / 1000.0
    pv = sprintf("☀️ %0.1f kW ", pvPower)
end
battery = ""
if batteryConfigured
    batteryPower = result["batteryPower"] / 1000.0
    batterySoC = result["batterySoC"]
    battery = batteryPower < 0 ? sprintf("🔋◀ %0.1f kW", batteryPower * -1.0) : sprintf("🔋▶ %0.1f kW", batteryPower)
    battery += sprintf(" (%d%%) ", batterySoC)
end

if batteryConfigured
    puts "#{home}#{grid}"
    puts "#{pv}#{battery}"    
else
    puts "#{home}#{pv}#{grid}"
end

puts "---"

# Loadpoints

activeLoadpoints = result["loadpoints"].select {|lp| lp["connected"] }

activeLoadpoints.each do |lp|
    chargePower = lp["chargePower"] / 1000.0
    vehicleSoC = lp["vehicleSoC"]
    vehicleRange = lp["vehicleRange"]
    chargedEnergy = lp["chargedEnergy"] / 1000.0
    vehicleTitle = lp["vehicleTitle"] 

    puts "🚘 #{vehicleTitle}"
    if lp["hasVehicle"]
        puts " ↳ 🔋 %d%% (%d km) | trim=false" % [vehicleSoC, vehicleRange]
    end
    puts " ↳ ⚡️ %0.1f kW #{PHASES[lp["activePhases"]]} (+%0.1f kWh) | trim=false" % [chargePower, chargedEnergy]
    puts "---"
end

if activeLoadpoints.length == 0
    puts "no vehicles connected"
    puts "---"
end

# Savings

savingsTotalCharged = result["savingsTotalCharged"] || 0
savingsSelfConsumptionCharged = result["savingsSelfConsumptionCharged"] || 0
savingsAmount = result["savingsAmount"] || 0
energyPrice = result["savingsEffectivePrice"] || result["tariffGrid"]
selfPercent = result["savingsSelfConsumptionPercent"] || 0

puts "☀️ %d%% (∑ %0.1f of %0.1f kWh)" % [selfPercent, savingsSelfConsumptionCharged, savingsTotalCharged]
puts "💰 %0.2f€ (%0.2f ct/kWh)" % [savingsAmount, energyPrice]
puts "open evcc| href=#{ENV['VAR_SERVER']}"
