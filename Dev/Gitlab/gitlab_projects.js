#!/usr/bin/env node
// jshint asi:true
// <bitbar.title>GITLAB projects</bitbar.title>
// <bitbar.version>v1.0</bitbar.version>
// <bitbar.author>Sylvain Baronnet</bitbar.author>
// <bitbar.author.github>sylvainbaronnet</bitbar.author.github>
// <bitbar.desc>List of your last active projects</bitbar.desc>
// <bitbar.dependencies>node.js</bitbar.dependencies>
// <bitbar.image>http://i.imgur.com/4X40XIK.png</bitbar.image>

var https         = false;
var gitlab_domain = '';
var private_token = '';
var default_page  = '/activity';
var order_by      = 'last_activity_at';
var nb_project    = '30';
var font_size     = '15';

var options = {
  host: gitlab_domain,
  path: '/api/v3/projects?order_by=' + order_by + '&sort=desc&per_page=' + nb_project,
  headers: {'PRIVATE-TOKEN': private_token}
};

console.log('GITLAB');
console.log('---');

var request_callback = function(res) {
  var body = ''
  res.on('data', function (data) {
    body += data
  })
  res.on('end', function () {
    var projects = JSON.parse(body);

    for(var p in projects) {
      var P = projects[p];

      var ta = timeago();
      var name = P.name;
      var last_activity = ta.ago(P.last_activity_at);
      var web_url = P.web_url;

      console.log(name + ' ⤏ ' + last_activity + ' | href="' + web_url + default_page + '" size=' + font_size);
      console.log(name + ' ⤏ ' + P.open_issues_count + ' issue' + (P.open_issues_count > 1 ? 's' : '') + ' | alternate=true href="' + web_url + '/issues" size=' + font_size);
    }
  })
};

var request;

if (https) {
  request = require('https').get(options, request_callback);
} else {
  request = require('http').get(options, request_callback);
}

request.end();

/* Source : https://github.com/digplan/time-ago */
var timeago = function() {

  var o = {
    second: 1000,
    minute: 60 * 1000,
    hour: 60 * 1000 * 60,
    day: 24 * 60 * 1000 * 60,
    week: 7 * 24 * 60 * 1000 * 60,
    month: 30 * 24 * 60 * 1000 * 60,
    year: 365 * 24 * 60 * 1000 * 60
  };
  var obj = {};

  obj.ago = function(nd) {
    var r = Math.round,
      pl = function(v, n) {
        return n + ' ' + v + (n > 1 ? 's' : '') + ' ago'
      },
      ts = new Date().getTime() - new Date(nd).getTime(),
      ii;
      if(ts < 0) {
        return 'Just now';
      }

    for (var i in o) {
      if (r(ts) < o[i]) return pl(ii || 'm', r(ts / (o[ii] || 1)))
      ii = i;
    }
    return pl(i, r(ts / o[i]));
  }
  return obj;
}

