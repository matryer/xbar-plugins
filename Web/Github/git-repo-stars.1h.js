#!/usr/local/bin/node
/*
* <bitbar.title>Github Repo Stars</bitbar.title>
* <bitbar.version>v1.0</bitbar.version>
* <bitbar.author>Varun Malhotra</bitbar.author>
* <bitbar.author.github>softvar</bitbar.author.github>
* <bitbar.image>http://varunmalhotra.xyz/media/images/bitbar-plugin-github-repos-stars.png</bitbar.image>
* <bitbar.desc>Github repos stars and Emoji notification 🔔 on change in stars count</bitbar.desc>
* <bitbar.dependencies>node</bitbar.dependencies>
* <bitbar.abouturl>http://varunmalhotra.xyz/blog/2016/02/bitbar-plugins-for-github-and-producthunt.html</bitbar.abouturl>
*/


// Configurable params
var config = {
    accessToken: '',
    username: 'softvar',
    hideZeroStarsRepo: false,
    filename: '/tmp/github-repo-stars.txt'
};

var https = require('https');
var fs = require('fs');

var displayInMenuBar = 'Git| color=green dropdown=false';
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

function getRepoStars(options) {
    var output = '';
    var req = https.request(options, function(res) {
        var body = '';
        res.on('data', function(chunk) {
            body += chunk;
        });
        res.on('end', function() {
            var name;
            var stars = [];
            var response = JSON.parse(body);

            // sort on stargazers_count to show repos having more stars on top
            sortOn(response, 'stargazers_count', true);

            for (var i = 0; i < response.length; i++) {
                // show only `your` repos, not the forked ones
                if (response[i].fork) { continue; }

                if (response[i].stargazers_count === 0 && config.hideZeroStarsRepo) { continue; }

                name = response[i].name;
                output += '(' + response[i].stargazers_count + ')' + name + ' | length=15 href=' + response[i].html_url;
                output += '\n---\n';
                stars.push(response[i].stargazers_count);
            }

            var oldData;
            fs.readFile(config.filename, 'utf8', function (err, data) {
                if (err) { throw err; }
                oldData = data;

                // SHow notification bell in OS X menu bar if current and revious data doesnt match
                // Voilla, someone starred your repo on Github
                if (oldData.toString() !== stars.toString()) {
                    displayInMenuBar = 'Git🔔| color=green dropdown=false';
                    displayInMenuBar += '\n---\n';
                }

                // Write all the stars count in the file
                fs.writeFile(config.filename, stars.toString(), {flag: 'w'}, function (err) {
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

checkIfFileExists();

// API call options
var options = {
    hostname: 'api.github.com',
    path: '/users/' + config.username + '/repos',
    method: 'GET',
    headers: {
        'Authorization': 'token ' + config.accessToken,
        'User-Agent': 'Awesome-Octocat-App'
    }
};

getRepoStars(options);
