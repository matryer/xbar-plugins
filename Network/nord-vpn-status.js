#!/usr/local/bin/node

// <xbar.title>NordVPN Status</xbar.title>
// <xbar.version>v1.0</xbar.version>
// <xbar.author>Dustin McBride</xbar.author>
// <xbar.author.github>dustinmcbride</xbar.author.github>
// <xbar.desc>Your nordVPN protected status for bit bar</xbar.desc>
// <xbar.image>https://raw.githubusercontent.com/dustinmcbride/bitbar-nord-vpn-status/master/screenshots/protected.png</xbar.image>
// <xbar.dependencies>node</xbar.dependencies>
// <xbar.abouturl>https://github.com/dustinmcbride/bitbar-nord-vpn-status</xbar.abouturl>

'use strict';

var nordUri = 'https://nordvpn.com/wp-admin/admin-ajax.php?action=get_user_info_data';
var https = require('https');

function createOutput (res) {
  var titleMessage = res.status ? 'NordVPN | color=green' : '⛔ NordVPN | color=red';
  var statusMessage = res.status ? 'Protected' : 'Unprotected';
  
  console.log(titleMessage);
  console.log('---');
  console.log('Status: ' + statusMessage);
  console.log('ISP: ' + res.isp);
  console.log('IP: ' + res.ip);
}

https.get(nordUri, function (res) {
  res.setEncoding('utf8');
  var body = '';
  res.on('data', function (data) {
    body += data;
  });
  res.on('end', function () {
    createOutput(JSON.parse(body));
  });
});

