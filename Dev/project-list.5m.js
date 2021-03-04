#!/usr/bin/env /usr/local/bin/node

// <bitbar.title>Semaphore CI</bitbar.title>
// <bitbar.version>v1.0</bitbar.version>
// <bitbar.author>Goran Gajic</bitbar.author>
// <bitbar.author.github>gorangajic</bitbar.author.github>
// <bitbar.desc>List your project and their statuses, from semaphoreci.com</bitbar.desc>
// <bitbar.dependencies>node.js</bitbar.dependencies>
// <bitbar.image>http://i.imgur.com/tRd1clI.png</bitbar.image>

var https = require('https');
var AUTH_TOKEN = ''; // YOUR AUTH TOKEN
var url = 'https://semaphoreci.com/api/v1/projects?auth_token='+AUTH_TOKEN;
var icon = '✅';

function status(result) {
    if (result === 'passed') {
        return '✅';
    }

    if (result === 'pending') {
        return '🌀';
    }
    return '🔴';
}

function color(result) {
    if (result === 'passed') {
        return 'green';
    }
    if (result === 'pending') {
        return 'orange';
    }

    return 'red';
}

function branchesOutput(branches) {
    return branches.map(function (branch){
        if (branch.result === 'failed') {
            icon = '🔴';
        }
        return ['- ', branch.branch_name,' ',status(branch.result), ' | color=', color(branch.result), ' href=', branch.build_url].join('');
    }).join('\n');
}

function handleResponse(body) {
    var output = body.map(function(project){
        return [project.owner, '/', project.name, ' | href=', project.html_url,'\n', branchesOutput(project.branches)].join('');
    }).join('\n---\n');
    console.log(icon + '\n---\n ⚙ Semaphoreci | href=https://semaphoreci.com/ \n---\n' + output);
}

https.get(url, function(res) {
    var body = '';
    res.on('data', function(data) {
        body += data;
    });
    res.on('end', function() {
        handleResponse(JSON.parse(body));
    });
});

