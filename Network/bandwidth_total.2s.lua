#!/usr/local/bin/lua

-- 1. install lua with e.g. "brew install lua"
-- 2. use this plugin

-- Written in lua, this plugin is smaller and faster than shell versions with similar function.

-- <bitbar.title>Bandwidth Total</bitbar.title>
-- <bitbar.version>v1.0.0</bitbar.version>
-- <bitbar.author>Charl P. Botha</bitbar.author>
-- <bitbar.author.github>cpbotha</bitbar.author.github>
-- <bitbar.desc>Displays total TX and RX KBytes/s for all active interfaces. Lua = smaller than .sh.</bitbar.desc>
-- <bitbar.dependencies>ifstat, lua</bitbar.dependencies>
-- <bitbar.image>https://cpbotha.net/thingies/bitbar_bandwidth_total_lua.jpg</bitbar.image>

-- to find the network interface connected to default route you could use this:
-- https://superuser.com/a/627581/130835

-- use ifstat to read current network throughput
-- example output:
--        en0                 en10               Total
-- KB/s in  KB/s out   KB/s in  KB/s out   KB/s in  KB/s out
-- 20.67      0.00     17.54      0.00     38.21      0.00
local file = io.popen('/usr/local/bin/ifstat -n -w -z -T 0.1 1')
local output = file:read('*all')

-- split into lines
-- https://stackoverflow.com/a/32847589/532513
local lines = {}
for l in output:gmatch("[^\r\n]+") do
    table.insert(lines, l)
end

local speeds = {}
for speed in lines[3]:gmatch("%S+") do
    table.insert(speeds, speed)
end

local ifaces = {}
for iface in lines[1]:gmatch("%S+") do
    table.insert(ifaces, iface)
end

-- lua tables start from 1
-- #name is the number of elements, so name[#name] will give you the last element
print(string.format("▼%.2f ▲%.2f", speeds[#speeds-1], speeds[#speeds]))
print("---")

for i = 1,#ifaces-1 do
    -- remember lua is 1-based (hence the *2+1 and *2+2)
    -- speeds are already %.2f formatted strings, or "n/a" if iface not available
    print(string.format("%s: ▼%s ▲%s", ifaces[i], speeds[i*2-1], speeds[i*2]))
end
