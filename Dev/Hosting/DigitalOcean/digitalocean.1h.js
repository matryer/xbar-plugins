#!/usr/bin/env /usr/local/bin/node

/* jshint esversion: 6 */

/*
 * <bitbar.title>DigitalOcean</bitbar.title>
 * <bitbar.version>v1.0</bitbar.version>
 * <bitbar.author>Hasit Mistry</bitbar.author>
 * <bitbar.author.github>hasit</bitbar.author.github>
 * <bitbar.image>http://i.imgur.com/1YTWFSn.png</bitbar.image>
 * <bitbar.desc>View status of DigitalOcean Droplets</bitbar.desc>
 * <bitbar.dependencies>node</bitbar.dependencies>
 */

var https = require('https');

var hostname = 'api.digitalocean.com';

// Add your Digital Ocean token. 
// If you have different DO teams you can add multiple tokens: ['DO_TOKEN_1', 'DO_TOKEN_2', ...]
var DOTokens = ['DO_TOKEN'];

var menuBarIcon = '💧';
var startMenu = '---';

console.log(menuBarIcon);
console.log(startMenu);

for (var i = 0; i < DOTokens.length; i++) {
    if (DOTokens.length == 1 && DOTokens[0] === 'DO_TOKEN') {
        console.log('DO token is missing!');
    } else {
        tokenIndex = i;
        getDroplets();
    }
}

function getDroplets() {
    performRequest('/v2/droplets', 'GET', function(data) {
        var droplets = data.droplets;

        for (var i = 0; i < droplets.length; i++) {
            var status = 'iVBORw0KGgoAAAANSUhEUgAAAAwAAAAMCAMAAABhq6zVAAAAXVBMVEUAAADAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvAOSvGu33lAAAAHnRSTlMAAwYHCQoQEh0fNVBRX2N5fJG+wMfK3O3v8fP3+/37J2BkAAAATklEQVQIHQXBhQHCAADAsA53d1j+P5OkqoaqqmbHr/G8rGr1BuOmmjwB47L2AM51A/CrFwBDFwDf2gI41fAAfObV4gFe66qG3fV9P0yrPxEfCr3MVhkLAAAAAElFTkSuQmCC';
            var date = new Date(droplets[i].created_at);
            var humanDate = date.getFullYear() + '/' + date.getMonth() + '/' + date.getDate() + ' ' + date.getHours() + ':' + date.getMinutes();

            if (droplets[i].status === 'active') {
                status = 'iVBORw0KGgoAAAANSUhEUgAAAAwAAAAMCAMAAABhq6zVAAAAXVBMVEUAAAAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmAnrmCynqnaAAAAHnRSTlMAAwYHCQoQEh0fNVBRX2N5fJG+wMfK3O3v8fP3+/37J2BkAAAATklEQVQIHQXBhQHCAADAsA53d1j+P5OkqoaqqmbHr/G8rGr1BuOmmjwB47L2AM51A/CrFwBDFwDf2gI41fAAfObV4gFe66qG3fV9P0yrPxEfCr3MVhkLAAAAAElFTkSuQmCC';
            }

            printBitBarLine(droplets[i].name, ['color=black', 'image=' + status]);

            // Open in browser and console
            printBitBarLine('--Open in browser', ['href=https://cloud.digitalocean.com/droplets/' + droplets[i].id]);
            printBitBarLine('--Connect to console', ['href=https://cloud.digitalocean.com/droplets/' + droplets[i].id + '/console?no_layout=true']);

            // Droplet creation date and time
            printBitBarLine('-----');
            printBitBarLine('--Created');
            printBitBarLine('--' + humanDate, ['color=black']);

            // Droplet IPv4 address
            printBitBarLine('-----');
            printBitBarLine('--IP Address');
            printBitBarLine('--' + droplets[i].networks.v4[0].ip_address, ['color=black']);

            // Droplet region
            printBitBarLine('-----');
            printBitBarLine('--Region');
            printBitBarLine('--' + droplets[i].region.name, ['color=black']);

            // Droplet details - image, memory, storage, vcpus
            printBitBarLine('-----');
            printBitBarLine('--Details');
            printBitBarLine('--Image: ' + droplets[i].image.distribution + droplets[i].image.name, ['color=black']);
            printBitBarLine('--Memory: ' + droplets[i].memory + 'MB', ['color=black']);
            printBitBarLine('--Storage: ' + droplets[i].disk + 'GB', ['color=black']);
            printBitBarLine('--CPUs: ' + droplets[i].vcpus, ['color=black']);
        }
    });
}

function printBitBarLine(title, args) {
    var lineArgs = [];

    if (title !== '-----') {
        for (var i in args) {
            lineArgs.push(args[i]);
        }

        if (lineArgs.length !== 0) {
            title = title + '|';
        }
    }

    console.log(title + lineArgs.join(' '));
}

function performRequest(endpoint, method, success) {
    var headers = {};

    headers = {
        'Authorization': 'Bearer ' + DOTokens[tokenIndex],
        'Content-Type': 'application/json'
    };

    var options = {
        host: hostname,
        path: endpoint,
        method: method,
        headers: headers
    };

    var req = https.request(options, function(res) {
        res.setEncoding('utf-8');

        var responseString = '';

        res.on('data', function(data) {
            responseString += data;
        });

        res.on('end', function() {
            var responseObject = JSON.parse(responseString);
            success(responseObject);
        });
    });

    req.end();

    req.on('error', function(err) {
        console.log(err);
    });
}
