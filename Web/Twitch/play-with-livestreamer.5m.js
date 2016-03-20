#!/usr/bin/env /usr/local/bin/node

// <bitbar.title>Twitch Livestreamer</bitbar.title>
// <bitbar.version>v1.0</bitbar.version>
// <bitbar.author>Goran Gajic</bitbar.author>
// <bitbar.author.github>gorangajic</bitbar.author.github>
// <bitbar.desc>list top 10 twitch streams for provided GAME, defaults to Dota 2</bitbar.desc>
// <bitbar.dependencies>node.js, livestreamer</bitbar.dependencies>
// <bitbar.image>http://i.imgur.com/XEQQaxC.png</bitbar.image>


var GAME = "Dota 2"; // game you want to fetch streams for
var LIMIT = 10; // streams limit
var url = 'https://api.twitch.tv/kraken/streams/?limit=' + LIMIT + '&game=' + encodeURIComponent(GAME);
var icon = '👾';
var LIVESTREAMER_PATH = '/usr/local/bin/livestreamer';

function handleResponse(body) {
    var output = body.streams.map(function(stream){
        var channel = stream.channel;
        var url = channel.url.replace('http://', '');
        var status = channel.status.replace(/\|/g, '').substr(0,40) + '...';
        return [status, '| size=9 \n', channel.display_name, ' - ', stream.viewers, ' | size=12 terminal=false bash=' + LIVESTREAMER_PATH + ' param1=', url, ' param2=best\n'].join('');
    }).join('\n---\n');
    console.log(icon + '\n---\n' + output);
}


var https = require('https');
https.get(url, function(res) {
    var body = '';
    res.on('data', function(data) {
        body += data;
    });
    res.on('end', function() {
        handleResponse(JSON.parse(body));
    });
});
