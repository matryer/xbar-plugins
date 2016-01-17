#!/usr/bin/env /usr/local/bin/node

/*
 * author:
 *     Goran Gajic - https://github.com/gorangajic
 * screenshot:
 *     http://i.imgur.com/XEQQaxC.png
 * desc:
 *     list top 10 streams for Dota 2
 *     you can modify the game by changing GAME variable
 *     and limit by changing LIMIT variable
 *     and place it in AUTH_TOKEN varibale
 */

var GAME = "Dota 2";
var LIMIT = 10;
var url = 'https://api.twitch.tv/kraken/streams/?limit=' + LIMIT + '&game=' + encodeURIComponent(GAME);
var icon = '👾';

function handleResponse(body) {
    var output = body.streams.map(function(stream){
        var channel = stream.channel;
        var url = channel.url.replace('http://', '');
        var command = '"livestreamer ' + url + ' best"';
        var status = channel.status.replace(/\|/g, '').substr(0,40) + '...';
        return [status, '| size=9 \n', channel.display_name, ' - ', stream.viewers, ' | size=12 bash=', command, ' \n'].join('');
    }).join('\n---\n');
    console.log(icon + '\n---\n' + output);
}


var https = require('https');
https.get(url, function(res) {
    var body = ''
    res.on('data', function(data) {
        body += data;
    });
    res.on('end', function() {
        handleResponse(JSON.parse(body));
    });
});

