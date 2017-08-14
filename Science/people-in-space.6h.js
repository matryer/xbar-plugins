#!/usr/bin/env /usr/local/bin/node

// <bitbar.title>People In Space</bitbar.title>
// <bitbar.version>v1.0</bitbar.version>
// <bitbar.author>Mullwar</bitbar.author>
// <bitbar.author.github>mullwar</bitbar.author.github>
// <bitbar.desc>How many people are in Space right now?</bitbar.desc>
// <bitbar.image>http://i.imgur.com/i9biB3R.png</bitbar.image>
// <bitbar.dependencies>node</bitbar.dependencies>
// <bitbar.abouturl>https://github.com/mullwar/my-bitbar-plugins</bitbar.abouturl>

const http = require('http');

function request(host, path) {
    return new Promise((resolve, reject) => {
        http.request({
            host, path,
            method: 'GET'
        }, (resp) => {
            let body = '';
            resp.setEncoding('utf8');
            resp.on('data', (chunk) => body += chunk);
            resp.on('end', () => resolve(JSON.parse(body)));
        }).end();
    });
}

function flag(name) {
    switch(name.toLowerCase()) {
        case 'russia': return '🇷🇺'
        case 'usa': return '🇺🇸'
        case 'italy': return '🇮🇹'
        case 'china': return '🇨🇳'
        case 'japan': return '🇯🇵'
        case 'canada': return '🇨🇦'
        case 'france': return '🇫🇷'
        case 'germany': return '🇩🇪'
        case 'belgium': return '🇧🇪'
        case 'netherlands': return '🇳🇱'
        case 'sweden': return '🇸🇪'
    } 
}

request('www.howmanypeopleareinspacerightnow.com', '/space.json').then((body) => {
    console.log(`👨🏻‍🚀 ${body.number}\n---`);
    body.people.map((person) => {
        console.log(`${flag(person.country)} ${person.name} | href=${person.bio} color=black`);
        console.log(`${person.title} – ${person.launchdate.split('-').reverse().join('.')}`);
        console.log(`---`);
    });
});
