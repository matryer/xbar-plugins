#!/usr/bin/env /usr/local/bin/node

/*
    <xbar.title>VPN Country Flag</xbar.title>
    <xbar.version>v0.1</xbar.version>
    <xbar.author>Miguel Laginha</xbar.author>
    <xbar.author.github>brecke</xbar.author.github>
    <xbar.desc>It shows the country flag corresponding to your IP Address (useful when using VPNs).</xbar.desc>
    <xbar.image>https://user-images.githubusercontent.com/19879/30689083-d8e4d2bc-9eb7-11e7-8d83-d45a9079c287.png</xbar.image>
    <xbar.dependencies>node</xbar.dependencies>
    <xbar.abouturl>https://github.com/brecke/bitbar-vpn-flag</xbar.abouturl>
*/

var ipapi = require('ipapi.co');
var https = require('https');
var flag = require('country-code-emoji').flag;

var IPAddress;
https.get('https://ipapi.co/ip/', function(resp) {
    var body = '';
    resp.on('data', function(data) {
        body += data;
    });

    resp.on('end', function() {
		IPAddress = body;
	
		ipapi.location(function(country) {
			console.log(flag(country)); 
		}, IPAddress.toString(), '', 'country');
    });
});


