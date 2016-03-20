#!/usr/local/bin/node
/*
* <bitbar.title>ProductHunt Posts Votes Count</bitbar.title>
* <bitbar.version>v1.0</bitbar.version>
* <bitbar.author>Varun Malhotra</bitbar.author>
* <bitbar.author.github>softvar</bitbar.author.github>
* <bitbar.image></bitbar.image>
* <bitbar.desc>ProductHunt Posts Votes count and Emoji notification 🔔  on change in votes count</bitbar.desc>
* <bitbar.dependencies>node</bitbar.dependencies>
* <bitbar.abouturl>http://varunmalhotra.xyz/blog/2016/02/bitbar-plugins-for-github-and-producthunt.html</bitbar.abouturl>
*/

var https = require('https');
var fs = require('fs');

// Configurable params
// Either of userId or username should be provided
// Providing userId will save one extra call for getting userId from username since PH API works on userId
var config = {
    accessToken: '',
    username: null,
    userId: 294870,
    filename: '/tmp/product-hunt-bitbar-votes-data.txt'
};


var userId;
var displayInMenuBar = 'PH| color=#da552f dropdown=false';
displayInMenuBar += '\n---\n';

function checkIfFileExists() {
    fs.exists(config.filename, function (exists) {
        if (exists) {
            return;
        } else {
            fs.writeFile(config.filename, {flag: 'wx'}, 0, function (err, data) {
                if (err) { throw err; }
            });
        }
    });
}

function sortOn(arr, key, orderBy) {
    arr.sort(function (a, b) {
        return orderBy ? b[key] - a[key] : a[key] - b[key];
    });
}

function fetchPostVotes() {
    // Fetch votes on each posts
    var options = {
        hostname: 'api.producthunt.com',
        path: '/v1/users/' + userId + '/posts',
        method: 'GET',
        headers: {
         'Authorization': 'Bearer ' + config.accessToken
        }
    };
    var output = '';
    var req = https.request(options, function(res) {
        var body = '';
        res.on('data', function(chunk) {
            body += chunk;
        });
        res.on('end', function() {
            var name;
            var votes = [];
            var response = JSON.parse(body);
            var posts = response.posts;

            // sort response on votes_count to show posts having higher votes on top
            sortOn(posts, 'votes_count', true);

            for (var i = 0; i < posts.length; i++) {
                name = posts[i].name;
                output +=  '(' + posts[i].votes_count + ')' + name + ' | length=15 href=' + posts[i].discussion_url;
                output += '\n---\n';
                votes.push(posts[i].votes_count);
            }

            var oldData;
            fs.readFile(config.filename, 'utf8', function (err, data) {
                if (err) { throw err; }
                oldData = data;

                // SHow notification bell in OS X menu bar if current and previous data dont match
                // Voilla! someone upvoted your post on ProductHunt
                if (oldData.toString() !== votes.toString()) {
                    displayInMenuBar = 'PH🔔| color=#da552f dropdown=false';
                    displayInMenuBar += '\n---\n';
                }

                // Write all the votes count in the file
                fs.writeFile(config.filename, votes.toString(), function (err) {
                    if (err) { throw err; }
                });

                // Finally log the entire output
                console.log(displayInMenuBar + output);
            });
        });
    });
    req.end();

    req.on('error', function(e) {
        console.error(e);
    });
}

function getUserId(options) {
    var req = https.request(options, function(res) {
        var body = '';
        res.on('data', function(chunk) {
            body += chunk;
        });
        res.on('end', function() {
            var response = JSON.parse(body);
            if (response.error) {
                console.log('No such PH User Found');
                return;
            }
            // Update userId
            userId = response.user.id;
            console.log(userId);
            fetchPostVotes();
        });
    });

    req.end();

    req.on('error', function(e) {
        console.error(e);
    });
}

checkIfFileExists();

if (config.userId) {
    userId = config.userId;
    fetchPostVotes();
} else if (config.username) {
    // Since we need ProductHunt userId for fetching user related data, get userId
    // API call options
    var options = {
        hostname: 'api.producthunt.com',
        path: '/v1/users/' + config.username,
        method: 'GET',
        headers: {
            'Authorization': 'Bearer ' + config.accessToken
        }
    };
    getUserId(options);
} else {
    console.log('!PH| color=#da552f dropdown=false\n---\n Provide PH userId/username.');
}
