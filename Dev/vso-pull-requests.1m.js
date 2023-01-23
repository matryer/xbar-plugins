#!/usr/bin/env /usr/local/bin/node

// <xbar.title>VSO Pullrequests</xbar.title>
// <xbar.version>v1.0</xbar.version>
// <xbar.author>Jelle Kralt</xbar.author>
// <xbar.author.github>jellekralt</xbar.author.github>
// <xbar.desc>Lists open pull requests from VSO</xbar.desc>
// <xbar.dependencies>node</xbar.dependencies>
// <xbar.image>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</xbar.image>

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

