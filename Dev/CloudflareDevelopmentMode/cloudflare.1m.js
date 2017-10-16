#!/usr/bin/env /usr/local/bin/node


/* jshint esversion: 6 */

/*
 * <bitbar.title>Cloudflare Development Mode</bitbar.title>
 * <bitbar.version>v1.0</bitbar.version>
 * <bitbar.author>Aaron Crawford</bitbar.author>
 * <bitbar.author.github>aaroncrawford</bitbar.author.github>
 * <bitbar.image>https://i.imgur.com/nKXgmxg.png</bitbar.image>
 * <bitbar.desc>Enable or Disable Development Mode from BitBar.  Plugin will need to be edited to include your API key and email to work.</bitbar.desc>
 * <bitbar.dependencies>node</bitbar.dependencies>
 */

// CHANGE THESE AREAS
let email = 'CHANGE@ME.com'; // Cloudflare email
let key = 'CHANGE ME'; // Global API Key, not Origin CA Key - Located under "My Profile"
//


var https = require('https');
var data = {
    zones: []
};

var headers = {
    'X-Auth-Email': email,
    'X-Auth-Key': key,
    'Content-Type': 'application/json'
};

function getZones() {
    var options = {
        host: 'api.cloudflare.com',
        path: '/client/v4/zones?per_page=50',
        method: 'GET',
        headers: headers
    };
    return new Promise((resolve, reject) => {
        var res = https.request(options, (res) => {
            var responseString = '';

            res.setEncoding('utf-8');

            res.on('data', function(data) {
                responseString += data;
            });

            res.on('end', function() {
                var parse = JSON.parse(responseString);
                parse.result.map((val, i) => {
                    if(val.development_mode > 0) {
                        dev = true;
                    }
                    else {
                        dev = false;
                    }
                    data.zones.push({name:val.name, id:val.id, dev:dev});
                });
                resolve();
            });

        });
        res.end();
    });
}

function flipSwitch(status) {
    var flag = (status == true) ? 'on' : 'off';
    var options = {
        host: 'api.cloudflare.com',
        path: `/client/v4/zones/${process.argv[3]}/settings/development_mode`,
        method: 'PATCH',
        headers: headers
    };

    return new Promise((resolve, reject) => {
        var postData = { value: flag };

        var res = https.request(options, (res) => {
            var body = [];
            var responseString = '';

            res.setEncoding('utf-8');
            res.on('data', function(data) {
                responseString += data;
            });
            res.on('end', function() {
                var parse = JSON.parse(responseString);
                resolve();
            });
            res.on('error', function(err) {
                reject(err);
            });
        });
        res.write(JSON.stringify(postData));
        res.end();
    });
}

console.log('⚙️');
console.log('---');

if(process.argv[2] == 'triggerUpdate') {
    var status = (process.argv[4] == 'Enable') ? true : false;
    flipSwitch(status)
    .then(getZones())
    .then(() => {
        data.zones.map((val, i) => {
            var status = (val.dev) ? 'Disable' : 'Enable';
            var color = (val.dev) ? 'red' : 'black';
            console.log(`${val.name}| color=${color}`);
            console.log(`--${status}| bash=${process.argv[0]} param1=${process.argv[1]} param2=triggerUpdate param3=${val.id} param4=${status} refresh=true terminal=false`);
        });
    });

}
else {
    getZones().then(() => {
        data.zones.map((val, i) => {
            var status = (val.dev) ? 'Disable' : 'Enable';
            var color = (val.dev) ? 'red' : 'black';
            console.log(`${val.name}| color=${color}`);
            console.log(`--${status}| bash=${process.argv[0]} param1=${process.argv[1]} param2=triggerUpdate param3=${val.id} param4=${status} refresh=true terminal=false`);
        });
    });
}