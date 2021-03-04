#!/usr/bin/env /usr/local/bin/node

// <bitbar.title>VSO Pullrequests</bitbar.title>
// <bitbar.version>v1.0</bitbar.version>
// <bitbar.author>Jelle Kralt</bitbar.author>
// <bitbar.author.github>jellekralt</bitbar.author.github>
// <bitbar.desc>Lists open pull requests from VSO</bitbar.desc>
// <bitbar.dependencies>node</bitbar.dependencies>
// <bitbar.image>http://i.imgur.com/b3KTgcg.png</bitbar.image>

var https = require('https');

var USERNAME = 'vso_username';
var PASSWORD = 'vso_password';
var DOMAIN = 'vso_domain';
var COLLECTION = 'vso_collection';
var REPO = 'repository_id';

/**
 * Makes an HTTP GET call
 * @param {string} path
 */
function get(path) {
    // Use native promise in favor of library so there is no dependency
    return new Promise( function (resolve, reject) {
        var options = {
            host: DOMAIN,
            path: path,
            auth: USERNAME + ':' + PASSWORD,
            json: true
        };

        return https.get(options, function (res) {
            var body = '';
            res.on('data', function (data) {
                body += data;
            });
            res.on('end', function () {
                resolve(JSON.parse(body));
            });
            res.on('error', function (err) {
                reject(err);
            });
        });
    });

}

/**
 * Handles API calls response
 * @param {Object} repo - Repository data
 * @param {Object} pullRequests - Pull requests data
 */
function handleResponse(repo, pullRequests) {
    console.log('↓⤸ ' + pullRequests.length + ' | dropdown=false');
    console.log('---');
    pullRequests.forEach(function(pr) {
        console.log(pr.pullRequestId + ': ' + pr.title + ' | href=' + repo._links.web.href + '/pullrequest/' + pr.pullRequestId);
    });
    console.log('---');
    console.log('✚ Create PR | href=' + repo._links.web.href + '/pullrequests?_a=createnew');
}

// Use native promise in favor of library so there is no dependency
Promise.all([
    get('/' + COLLECTION + '/_apis/git/repositories/' + REPO),
    get('/' + COLLECTION + '/_apis/git/repositories/' + REPO + '/pullrequests')
]).then(function(data) {
    handleResponse(data[0], data[1].value);
});

