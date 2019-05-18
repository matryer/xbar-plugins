#!/usr/bin/env /usr/local/bin/node

// <bitbar.title>People In Space</bitbar.title>
// <bitbar.version>v1.1</bitbar.version>
// <bitbar.author>Mullwar</bitbar.author>
// <bitbar.author.github>mullwar</bitbar.author.github>
// <bitbar.desc>How many people are in Space right now?</bitbar.desc>
// <bitbar.image>http://i.imgur.com/i9biB3R.png</bitbar.image>
// <bitbar.dependencies>node</bitbar.dependencies>
// <bitbar.abouturl>https://github.com/mullwar/bitbar-plugins</bitbar.abouturl>

"use strict";

/* jshint -W100 */
/* jshint esversion: 6 */

const https = require('https');

const ENDPOINT = 'https://www.howmanypeopleareinspacerightnow.com/peopleinspace.json';

function request(endpoint) {
    return new Promise((resolve, reject) => {
        https.get(endpoint, (res) => {
            const body = [];
            res.setEncoding('utf8');
            res.on('data', (data) => body.push(data));
            res.on('end', () => {
                try {
                    resolve(JSON.parse(body.join()));
                } catch (error) {
                    reject(error);
                }
            });
            res.on('error', (error) => {
                reject(error);
            });
        });
    });
}

function getEmojiFlag(name) {
    switch (name.toLowerCase()) {
        case 'russia': return '🇷🇺';
        case 'usa': return '🇺🇸';
        case 'italy': return '🇮🇹';
        case 'china': return '🇨🇳';
        case 'japan': return '🇯🇵';
        case 'canada': return '🇨🇦';
        case 'france': return '🇫🇷';
        case 'germany': return '🇩🇪';
        case 'belgium': return '🇧🇪';
        case 'netherlands': return '🇳🇱';
        case 'sweden': return '🇸🇪';
    }
}

request(ENDPOINT).then((json) => {
    console.log(`👨🏻‍🚀 ${json.number}`);
    console.log(`---`);
    json.people.forEach((person) => {
        console.log(`${getEmojiFlag(person.country)} ${person.name} | href=${person.biolink} color=black`);
        console.log(`${person.title} – ${person.launchdate.split('-').reverse().join('.')}`);
        console.log(`---`);
    });
}).catch((error) => {
    console.log(`👨🏻‍🚀 ?\n---`);
    console.log(`Houston, we have an error! | color=red`);
    console.log(`---`);
    console.log(error);
});
