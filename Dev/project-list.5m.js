#!/usr/bin/env /usr/local/bin/node

// <xbar.title>Semaphore CI</xbar.title>
// <xbar.version>v1.0</xbar.version>
// <xbar.author>Goran Gajic</xbar.author>
// <xbar.author.github>gorangajic</xbar.author.github>
// <xbar.desc>List your project and their statuses, from semaphoreci.com</xbar.desc>
// <xbar.dependencies>node.js</xbar.dependencies>
// <xbar.image>http://i.imgur.com/tRd1clI.png</xbar.image>

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

