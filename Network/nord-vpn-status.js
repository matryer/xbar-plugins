#!/usr/local/bin/node

// <bitbar.title>NordVPN Status</bitbar.title>
// <bitbar.version>v1.0</bitbar.version>
// <bitbar.author>Dustin McBride</bitbar.author>
// <bitbar.author.github>dustinmcbride</bitbar.author.github>
// <bitbar.desc>Your nordVPN protected status for bit bar</bitbar.desc>
// <bitbar.image>https://raw.githubusercontent.com/dustinmcbride/bitbar-nord-vpn-status/master/screenshots/protected.png</bitbar.image>
// <bitbar.dependencies>node</bitbar.dependencies>
// <bitbar.abouturl>https://github.com/dustinmcbride/bitbar-nord-vpn-status</bitbar.abouturl>

'use strict';
const nordUri = 'https://nordvpn.com/wp-admin/admin-ajax.php?action=get_user_info_data';
const https = require('https');

function createOutput (res) {
  console.log(`${res.status ? 'NordVPN | color=green' : '⛔ NordVPN | color=red'}`);
  console.log('---');
  console.log(`Status: ${res.status ? 'Protected' : 'Unprotected'}`);
  console.log(`ISP: ${res.isp}`);
  console.log(`IP: ${res.ip}`);
}

https.get(nordUri, res => {
  res.setEncoding('utf8');
  let body = '';
  res.on('data', data => {
    body += data;
  });
  res.on('end', () => {
    createOutput(JSON.parse(body));
  });
});




