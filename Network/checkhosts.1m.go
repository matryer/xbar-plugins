#! /usr/bin/env gorun

// <bitbar.title>Check hosts</bitbar.title>
// <bitbar.version>v1.0</bitbar.version>
// <bitbar.author>Christoph Schlosser</bitbar.author>
// <bitbar.author.github>christophschlosser</bitbar.author.github>
// <bitbar.desc>The script tries to open a connection to certain ports or addresses
// to check if a host is available. Available hosts can be opened directly from
// the drop down if they are up and a protocol is specified.</bitbar.desc>
// <bitbar.image>https://raw.githubusercontent.com/christophschlosser/bitbar-plugins/checkhosts/Network/checkhosts.png</bitbar.image>
// <bitbar.dependencies>go</bitbar.dependencies>

package main

import (
        "fmt"
        "net"
        "sync"
)

// List of hosts - customize this using the format below
var hosts = []host {
        {
                address: "github.com", // can be a FQDN
                isUp: false, // always default a host to false
                ports: []networkPort {
                        {
                                network: "tcp", // see the networkPort struct for
                                                // available networks
                                port: "443", // port that should be checked
                                protocol: "https", // the protocol used to open
                                                   // the connection from the bar
                                isUp: false, // same as host, always defaults to false
                        },
                        {
                                network: "tcp",
                                port: "80",
                                protocol: "http",
                                isUp: false,
                        },
                },
        },
        { // Mulitple hosts are possible
                address: "8.8.8.8", // can also be an ip address
                isUp: false,
                ports: []networkPort {
                        {
                                network: "udp",
                                port: "53",
                                protocol: "", // The protocol is optional, if there
                                              // is no application to open a connection
                                isUp: false,
                        },
                        {
                                // Using anything besides tcp or udp requires sudo!
                                // It will fail and show as down
                                network: "ip4",
                                port: "", // ports may also be optional with some types of network
                                protocol: "",
                                isUp: false,
                        },
                },
        },
}

// Display settings
// Only show a host as up if all ports are up as well
var upIfAllPorts = false

// Show number of reachable ports in menu bar text as well
var showPortTotal = true

// Show number of hosts up in menu bar text
// This string will always be used with 2 parameters, the first one being
// the number of hosts up, the second one is the total number of hosts.
// If showPort Total is true the same format for the ports will be appended to
// this string in parenthesis
var barText = "%d/%d"

// Color for host down
var colorDown = "red"
// Color for host up
var colorUp = "green"

// Implementation - No more configuration below here is necessary

// Structs
type networkPort struct {
        // Known networks are "tcp", "tcp4" (IPv4-only), "tcp6" (IPv6-only),
        // "udp", "udp4" (IPv4-only), "udp6" (IPv6-only)
        // The following networks are also known but work only if the script is
        // executed as root user:
        // "ip", "ip4" (IPv4-only), "ip6" (IPv6-only), "unix", "unixgram" and "unixpacket".
        network string
        port string
        protocol string
        isUp bool
}

type host struct {
        address string
        isUp bool
        ports []networkPort
}

// functions
func checkHost(hostNum int, portNum int) {
        conn, err := net.Dial(hosts[hostNum].ports[portNum].network,
                hosts[hostNum].address + ":" + hosts[hostNum].ports[portNum].port)
        if err == nil {
                hosts[hostNum].ports[portNum].isUp = true
                defer conn.Close()
        }
}

func setHostsUp() {
        for hostNum := 0; hostNum < len(hosts); hostNum++ {
                var portsDown int
                currHost := hosts[hostNum]
                for portNum := 0; portNum < len(currHost.ports); portNum++ {
                        currPort := currHost.ports[portNum]
                        if (!currPort.isUp) {
                                portsDown++
                        }
                }
                if (portsDown > 0) {
                       if (!upIfAllPorts) {
                                hosts[hostNum].isUp = true
                       }
                } else {
                        hosts[hostNum].isUp = true
                }
        }
}

func checkHosts() {
        var wg sync.WaitGroup

        for hostNum := 0; hostNum < len(hosts); hostNum++ {
                for portNum := 0; portNum < len(hosts[hostNum].ports); portNum++ {
                        wg.Add(1)
                        go func(hostNum int, portNum int) {
                                defer wg.Done()
                                checkHost(hostNum, portNum)
                        }(hostNum, portNum)
                }
        }
        wg.Wait()

        setHostsUp()
}

func print() {
        var hostsUp int
        var portsUp int
        var portsTotal int
        var display string
        for _, hostNum := range hosts {
                if (hostNum.isUp) {
                        hostsUp++
                }
                display += hostNum.address
                var hasLink bool
                for _, portNum := range hostNum.ports {
                        portsTotal++;
                        if (portNum.isUp) {
                                portsUp++
                        }

                        if (portNum.protocol != "") {
                                display += "|href=" + portNum.protocol + "://" + hostNum.address + ":" + portNum.port
                                hasLink = true
                                break
                        }
                }
                if (!hasLink) {
                        if (hostNum.isUp) {
                                display += "|color=" + colorUp
                        } else {
                                display += "|color=" + colorDown
                        }
                }
                display += "\n"
        }

        var barDisplay = barText + "\n"

        if (showPortTotal) {
                fmt.Printf(barText + " (" + barText + ")\n", hostsUp, len(hosts), portsUp, portsTotal)
        } else {
                fmt.Printf(barDisplay, hostsUp, len(hosts))
        }
        fmt.Println("---")
        fmt.Print(display)
}

func main() {
        checkHosts()
        print()
}