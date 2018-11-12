#!/usr/bin/env /usr/local/bin/node

// <bitbar.title>Gogs Issues</bitbar.title>
// <bitbar.version>v1.0</bitbar.version>
// <bitbar.author>Christoph Schlosser</bitbar.author>
// <bitbar.author.github>christophschlosser</bitbar.author.github>
// <bitbar.desc>Displays the number of issues from Gogs</bitbar.desc>
// <bitbar.image>https://raw.githubusercontent.com/christophschlosser/bitbar-plugins/gogs-issues/Dev/Gogs/gogs-issues.png</bitbar.image>
// <bitbar.dependencies>node.js</bitbar.dependencies>

// Set these
HOSTNAME = "gogs.example.cpm";
ACCESS_TOKEN = "youraccesstoken";

// Ignore everthing below

var https = require('https');

var options = {
    host: HOSTNAME,
    path: '/api/v1/user/issues?token=' + ACCESS_TOKEN
};

https.get(options, function (res) {
    var body = '';
    res.on('data', function (data) {
        body += data;
    });
    res.on('end', function () {
        handleResponse(JSON.parse(body));
    });
});

function allIssues(body) {
    var issues = body.map(function (issues) {
        return [issues.title, ' - #', issues.number, ' (', issues.user.login, ') | href=https://', HOSTNAME ,'/issues\n'].join('');
    }).join('\n');

    return issues;
}

function handleResponse(body) {
    var countIssues = body.length;
    var listIssues = allIssues(body);

    console.log("Gogs: " + countIssues + " Issues");
    console.log("---");
    console.log(listIssues);
}