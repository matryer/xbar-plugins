#!/usr/bin/env /usr/local/bin/node

// <bitbar.title>GITLAB projects</bitbar.title>
// <bitbar.version>v1.0</bitbar.version>
// <bitbar.author>Sylvain Baronnet</bitbar.author>
// <bitbar.author.github>sylvainbaronnet</bitbar.author.github>
// <bitbar.desc>List of your last active projects</bitbar.desc>
// <bitbar.dependencies>node.js</bitbar.dependencies>
// <bitbar.image>http://i.imgur.com/4X40XIK.png</bitbar.image>
var http = require('http');

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

var request = http.get(options, function (res) {
  var body = ''
  res.on('data', function (data) {
    body += data
  })
  res.on('end', function () {
    var projects = JSON.parse(body);

    for(var p in projects) {
      var P = projects[p];

      var name = P.name;
      var last_activity = timeSince(P.last_activity_at) + ' ago';
      var web_url = P.web_url;

      console.log(name + ' ⤏ ' + last_activity + ' | href="' + web_url + default_page + '" size=' + font_size);
      console.log(name + ' ⤏ ' + P.open_issues_count + ' issue' + (P.open_issues_count > 1 ? 's' : '') + ' | alternate=true href="' + web_url + '/issues" size=' + font_size);
    }
  })
});

request.end();


function timeSince(time) {

  var date = new Date((time || "").replace(/-/g,"/").replace(/[TZ]/g," "))

  var seconds = Math.floor((new Date() - date) / 1000);

  var interval = Math.floor(seconds / 31536000);

  if (interval > 1) {
      return interval + " years";
  }
  interval = Math.floor(seconds / 2592000);
  if (interval > 1) {
      return interval + " months";
  }
  interval = Math.floor(seconds / 86400);
  if (interval > 1) {
      return interval + " days";
  }
  interval = Math.floor(seconds / 3600);
  if (interval > 1) {
      return interval + " hours";
  }
  interval = Math.floor(seconds / 60);
  if (interval > 1) {
      return interval + "minutes";
  }
  return Math.floor(seconds) + " seconds";
}

