#!/usr/local/bin/node

// <bitbar.title>NordVPN Status</bitbar.title>
// <bitbar.version>v1.1</bitbar.version>
// <bitbar.author>Dustin McBride</bitbar.author>
// <bitbar.author.github>dustinmcbride</bitbar.author.github>
// <bitbar.desc>NordVPN protected status for bit bar</bitbar.desc>
// <bitbar.image>https://raw.githubusercontent.com/dustinmcbride/bitbar-nord-vpn-status/master/screenshots/protected.png</bitbar.image>
// <bitbar.dependencies>node</bitbar.dependencies>
// <bitbar.abouturl>https://github.com/dustinmcbride/bitbar-nord-vpn-status</bitbar.abouturl>

'use strict';
var https = require('https');
var nordUri = 'https://nordvpn.com/wp-admin/admin-ajax.php?action=get_user_info_data';

function createOutput (res, error) {
  var message = {
    title: error ? '!' : res.status ? 'NordVPN | color=green' : '⛔ NordVPN | color=red',
    status: error ? 'Connection Error' : res.status ? 'Protected' : 'Unprotected',
    isp: error ? 'none' : res.isp,
    ip: error ? 'none' : res.ip,
  };
  
  console.log(message.title);
  console.log('---');
  console.log('Status: ' + message.status);
  console.log('ISP: ' + message.isp);
  console.log('IP: ' + message.ip);
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
})
  .on('error', function (error) {
    createOutput(null, error);
});

