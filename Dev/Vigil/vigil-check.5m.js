#!/usr/bin/env /usr/local/bin/node

/*
  <bitbar.title>Vigil Website Monitoring</bitbar.title>
  <bitbar.version>v1.0</bitbar.version>
  <bitbar.author>Cameron Rye</bitbar.author>
  <bitbar.author.github>cameronrye</bitbar.author.github>
  <bitbar.desc>Displays the status of hosts being monitered by Vigil.</bitbar.desc>
  <bitbar.image>https://cameronrye.com/img/vigil-check.png</bitbar.image>
  <bitbar.dependencies>node</bitbar.dependencies>

  Vigil Website Monitoring
  by Cameron Rye (@cameronrye)

  Auth token can be scraped from the status board url @ http://client.vigil-app.com/?p=integrations&t=sb

*/

'use strict';

var https = require('https'),
    AUTH_TOKEN = 'YOUR AUTH TOKEN',
    url = 'https://api.vigil-app.com/v1/user/' + AUTH_TOKEN + '/host?populateHostMonitors';

function createBar(data) {
  var isDown = false,
      hosts = '',
      bitbar = '';

  hosts = data.hosts.map(function(host) {
    var vigil = '',
        protocol = '',
        method = '',
        time = '',
        params = '';

    if (!host.hostMonitors[0].lastMonitorResult.resultCode) isDown = true;

    time = new Date(host.hostMonitors[0].lastMonitorResult.endTimestamp);

    params = host.hostMonitors[0].parameters.map(function(param) {
      switch (param.monitorParameterName) {
        case 'monitors.webpage.protocol':
          protocol = (param.value === 'https') ? 'https://' : 'http://';
          break;
        case 'monitors.webpage.method':
          method = param.value;
          break;
      }
    });

    vigil  = host.hostMonitors[0].lastMonitorResult.resultCode ? ' ▲ ':' ▼ ';
    vigil += host.hostMonitors[0].name + '| href=' + protocol + host.hostName + '\n';
    vigil += method + '\u00A0\u00A0\u00A0' + protocol + host.hostName + '/\n';
    vigil += 'Status Code: ' + host.hostMonitors[0].lastMonitorResult.resultProperties['monitors.webpage.statusCode'];
    vigil += ' (' + getStatus(host.hostMonitors[0].lastMonitorResult.resultProperties['monitors.webpage.statusCode']) + ')\n';
    vigil += 'Response Time: ' + (host.hostMonitors[0].lastMonitorResult.resultProperties['monitors.webpage.stats.totalSeconds'] * 1000) + 'ms\n';
    vigil += 'Avg Response: ' + (host.hostMonitors[0].uptime.responseTimeMean * 1000) + 'ms\n';
    vigil += 'Uptime: ' + host.hostMonitors[0].uptime.uptimePercent + '%\n';
    vigil += time + '| size: 10';

    return vigil;

  }).join('\n---\n');

  bitbar  = isDown ? '▼':'▲';
  bitbar += '| dropdown=false\n---\n';
  bitbar += ' ▬\n---\n';
  bitbar += hosts;
  bitbar += '\n---\n ⚙ Vigil Website Monitoring | href=https://client.vigil-app.com/ \n---\n';

  console.log(bitbar);
}

function getStatus(code) {
  var status = '';
  switch (code) {
    case '100':
      status = 'Continue';
      break;
    case '101':
      status = 'Switching Protocol';
      break;
    case '200':
      status = 'OK';
      break;
    case '201':
      status = 'Created';
      break;
    case '202':
      status = 'Accepted';
      break;
    case '203':
      status = 'Non-Authoritative Information';
      break;
    case '204':
      status = 'No Content';
      break;
    case '205':
      status = 'Reset Content';
      break;
    case '206':
      status = 'Partial Content';
      break;
    case '300':
      status = 'Multiple Choice';
      break;
    case '301':
      status = 'Moved Permanently';
      break;
    case '302':
      status = 'Found';
      break;
    case '303':
      status = 'See Other';
      break;
    case '304':
      status = 'Not Modified';
      break;
    case '305':
      status = 'Use Proxy';
      break;
    case '306':
      status = 'Unused';
      break;
    case '307':
      status = 'Temporary Redirect';
      break;
    case '308':
      status = 'Permanent Redirect';
      break;
    case '400':
      status = 'Bad Request';
      break;
    case '401':
      status = 'Unauthorized';
      break;
    case '402':
      status = 'Payment Required';
      break;
    case '403':
      status = 'Forbidden';
      break;
    case '404':
      status = 'Not Found';
      break;
    case '405':
      status = 'Method Not Allowed';
      break;
    case '406':
      status = 'Not Acceptable';
      break;
    case '407':
      status = 'Proxy Authentication Required';
      break;
    case '408':
      status = 'Request Timeout';
      break;
    case '409':
      status = 'Conflict';
      break;
    case '410':
      status = 'Gone';
      break;
    case '411':
      status = 'Length Required';
      break;
    case '412':
      status = 'Precondition Failed';
      break;
    case '413':
      status = 'Payload Too Large';
      break;
    case '414':
      status = 'URI Too Long';
      break;
    case '415':
      status = 'Unsupported Media Type';
      break;
    case '416':
      status = 'Requested Range Not Satisfiable';
      break;
    case '417':
      status = 'Expectation Failed';
      break;
    case '418':
      status = 'I\'m a teapot';
      break;
    case '421':
      status = 'Misdirected Request';
      break;
    case '426':
      status = 'Upgrade Required';
      break;
    case '428':
      status = 'Precondition Required';
      break;
    case '429':
      status = 'Too Many Requests';
      break;
    case '431':
      status = 'Request Header Fields Too Large';
      break;
    case '500':
      status = 'Internal Server Error';
      break;
    case '501':
      status = 'Not Implemented';
      break;
    case '502':
      status = 'Bad Gateway';
      break;
    case '503':
      status = 'Service Unavailable';
      break;
    case '504':
      status = 'Gateway Timeout';
      break;
    case '505':
      status = 'HTTP Version Not Supported';
      break;
    case '506':
      status = 'Variant Also Negotiates';
      break;
    case '507':
      status = 'Variant Also Negotiates';
      break;
    case '511':
      status = 'Network Authentication Required';
      break;
  }
  return status;
}

https.get(url, function(res) {
  var json = '';
  res.on('data', function(data) {
    json = data;
  });
  res.on('end', function() {
    createBar(JSON.parse(json));
  });
});
