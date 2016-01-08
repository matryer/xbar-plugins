#!/usr/bin/env /usr/local/bin/node

/*
 * author:
 *     Goran Gajic - https://github.com/gorangajic
 * screenshot:
 *     http://i.imgur.com/BxKV8jU.png
 * desc:
 *     list your projects from semaphoreci.com
 *     grab your token from https://semaphoreci.com/users/edit
 *     and place it in AUTH_TOKEN varibale
 */

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
        if (branch.result === 'faield') {
            icon = '🔴';
        }
        return ['- ', branch.branch_name,' ',status(branch.result), ' | color=', color(branch.result), ' href=', branch.build_url].join('');
    }).join('\n');
}

function handleResponse(body) {
    var output = body.map(function(project){
        return [project.owner, '/', project.name, ' | color=white href=', project.html_url,'\n', branchesOutput(project.branches)].join('');
    }).join('\n---\n');
    console.log(icon + '\n---\n ⚙ Semaphoreci | color=white href=https://semaphoreci.com/ \n---\n' + output);
}

https.get(url, function(res) {
    var body = ''
    res.on('data', function(data) {
        body += data;
    });
    res.on('end', function() {
        handleResponse(JSON.parse(body));
    });
});
