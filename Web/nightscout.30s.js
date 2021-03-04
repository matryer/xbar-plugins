#!/usr/bin/env /usr/local/bin/node
/* jshint esversion: 6 */

/*
<bitbar.title>BitBar Nightscout Plugin</bitbar.title>
<bitbar.version>1.0</bitbar.version>
<bitbar.author>Charlie Chrisman</bitbar.author>
<bitbar.author.github>cachrisman</bitbar.author.github>
<bitbar.desc>Gets the most recent reading and trend arrow from nightscout</bitbar.desc>
<bitbar.image>https://raw.githubusercontent.com/cachrisman/bitbar-nightscout/master/screenshot.png</bitbar.image>
<bitbar.dependencies>node, npm, npm/fs, npm/got</bitbar.dependencies>
<bitbar.abouturl>https://github.com/cachrisman/bitbar-nightscout</bitbar.abouturl>
*/

// user settings
const HOSTNAME = 'your_nightscout_site.herokuapp.com';
const TOKEN = 'your_nightscout_api_token_with_read_access';
const BG_HIGH_LEVEL = 180;
const BG_LOW_LEVEL = 70;

// don't edit code below!

const fs = require('fs');
const got = require('got');

const requestUrl = `https://${HOSTNAME}/api/v1/entries.json?count=1&TOKEN=${TOKEN}`;
const ver = '1.0';

const symbols = {
  'NONE': '?',
  'DoubleUp': '⇈',
  'SingleUp': '↑',
  'FortyFiveUp': '↗',
  'Flat': '→',
  'FortyFiveDown': '↘',
  'SingleDown': '↓',
  'DoubleDown': '⇊',
  'NOT COMPUTABLE': '??',
  'RATE OUT OF RANGE': '⇕'
};

const data_dir = __dirname + '/nightscout/';
if (!fs.existsSync(data_dir)) fs.mkdirSync(data_dir);
const last_reading_file = data_dir + 'last_reading';

function getLastReading() {
  let last_reading;
  try {
    last_reading = JSON.parse(fs.readFileSync(last_reading_file, 'utf8'))[0];
  } catch (e) {
    logger('error.log', JSON.stringify(e));
    last_reading = {_id:''};
  }
  return last_reading;
}

function logger(file, data) {
  if (fs.existsSync(data_dir + file)) 
    fs.appendFileSync(data_dir + file, data + '\n');
  else 
    fs.writeFileSync(data_dir + file, data + '\n');
}

function writeOutput(data) {
  let glucose = parseInt(data.sgv);
  let date = new Date(data.date);
  let trend = data.trend;
  let direction = data.direction;

  let top_line = `BG: ${glucose} ${symbols[direction]}`;

  if (glucose > BG_HIGH_LEVEL)
    top_line += ' | color=yellow';
  else if (glucose < BG_LOW_LEVEL)
    top_line += ' | color=red';

  console.log(`${top_line}
---
Last Reading: ${date.toLocaleString()}
Blood Glucose: ${glucose}
Trend: ${trend}
Direction: ${direction} ${symbols[direction]}
---
Refresh | refresh=true terminal=false
---
BitBar Nightscout Plugin v${ver}
--by Charlie Chrisman | href=http://www.charliechrisman.com
-----
--GitHub Page | href=https://github.com/cachrisman/bitbar-nightscout
--Changelog | href=https://github.com/cachrisman/bitbar-nightscout/blob/master/CHANGELOG.md`);
}

got(requestUrl).then(response => {
  let last_reading = getLastReading();
  let data = JSON.parse(response.body)[0];
  if (data._id !== last_reading._id) {
    logger('readings.log', response.body);
    fs.writeFileSync(last_reading_file, response.body);
  }
  writeOutput(data);
}).catch(err => {
  writeOutput(last_reading);
  logger('error.log', `${new Date()} - ${JSON.stringify(err)}`);
});
