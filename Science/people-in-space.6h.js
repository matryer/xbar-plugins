#!/usr/bin/env /usr/local/bin/node

// <xbar.title>People In Space</xbar.title>
// <xbar.version>v1.2</xbar.version>
// <xbar.author>Mullwar</xbar.author>
// <xbar.author.github>mullwar</xbar.author.github>
// <xbar.desc>How many people are in Space right now?</xbar.desc>
// <xbar.image>http://i.imgur.com/i9biB3R.png</xbar.image>
// <xbar.dependencies>node</xbar.dependencies>
// <xbar.abouturl>https://github.com/mullwar/bitbar-plugins</xbar.abouturl>

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
        console.log(`${getEmojiFlag(person.country)} ${person.name} | href=${person.biolink}`);
        console.log(`${person.title} – ${person.launchdate.split('-').reverse().join('.')}`);
        console.log(`---`);
    });
}).catch((error) => {
    console.log(`👨🏻‍🚀 ?\n---`);
    console.log(`Houston, we have an error! | color=red`);
    console.log(`---`);
    console.log(error);
});
