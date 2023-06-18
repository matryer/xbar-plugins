#!/usr/bin/env node

// <xbar.title>GitHub PRNotii</xbar.title>
// <xbar.version>v1.0</xbar.version>
// <xbar.author>aiden.ahn</xbar.author>
// <xbar.author.github>eu81273</xbar.author.github>
// <xbar.desc>Check Github Pull Reuqest</xbar.desc>
// <xbar.image>https://github.com/matryer/xbar-plugins/assets/2687916/d9c8dd73-d0e9-4812-b0f4-2a57e478a173</xbar.image>

// <swiftbar.hideAbout>true</swiftbar.hideAbout>
// <swiftbar.hideRunInTerminal>true</swiftbar.hideRunInTerminal>
// <swiftbar.hideLastUpdated>false</swiftbar.hideLastUpdated>
// <swiftbar.hideDisablePlugin>true</swiftbar.hideDisablePlugin>
// <swiftbar.hideSwiftBar>true</swiftbar.hideSwiftBar>

const https = require('https');
const {join} = require('path');
const {homedir} = require('os');
const {readFile} = require('fs/promises');

const request = (url, headers) => new Promise((resolve, reject) => {
  const body = [];
  const req = https.get(url, {headers}, res => {
    res.on('data', chunk => body.push(chunk));
    res.on('end', () => resolve(Buffer.concat(body).toString()));
  });
  req.on('error', reject);
  req.end();
  setTimeout(reject, 5000, new Error('PRNOTII_ETIMEDOUT'));
});
const refine = items => items.map(item => ({
  repository: item.repository_url.replace(/.+\//, ''),
  href: item.html_url,
  title: item.title,
}));
const organize = refined => refined.reduce((prev, item) => {
  prev[item.repository] = prev[item.repository] || [];
  prev[item.repository].push(item);
  return prev;
}, {});
const githubQuery = (endpoint, token, query) => 
  request(endpoint + '/search/issues?q=' + encodeURIComponent(query), {'Authorization': 'token ' + token})
    .then(JSON.parse)
    .then(res => refine(res.items))
    .then(organize);
const subMenu = organized => Object.entries(organized)
  .map(([repository, items]) => '--' + repository + ' | disabled=true | size=12\n' + items.map(item => '--' + item.title + ' | href=' + item.href).join('\n'))
  .join('\n');
const printOut = str => str && console.log(str);

(async function () {
  try {
    const configFile = join(homedir(), '.prnotii');
    const {endpoint, token} = await readFile(configFile, {encoding: 'utf8'}).then(JSON.parse);

    const requested = await githubQuery(endpoint, token, 'is:open is:pr user-review-requested:@me');
    const requestedSubmenu = subMenu(requested);
    printOut(`● | color=${Object.keys(requested).length ? 'red' : 'white'}`);
    printOut('---');
    printOut('👩‍💻 Review Requested');
    printOut(requestedSubmenu);

    const reviewed = await githubQuery(endpoint, token, 'is:pr is:open reviewed-by:@me');
    const reviewedSubmenu = subMenu(reviewed);
    printOut('💬 Reviewed');
    printOut(reviewedSubmenu);

    const created = await githubQuery(endpoint, token, 'is:open is:pr author:@me');
    const createdSubmenu = subMenu(created);
    printOut('💪🏻 Created');
    printOut(createdSubmenu);

    const mentioned = await githubQuery(endpoint, token, 'is:open is:pr mentions:@me');
    const mentionedSubmenu = subMenu(mentioned);
    printOut('😀 Mentioned');
    printOut(mentionedSubmenu);
  } catch (e) {
    printOut(`● | color=yellow`);
    printOut('---');

    switch (true) {
      case e.message.includes('ENOENT'):
        printOut('🌈 HOW TO SETUP');
        printOut('---');
        printOut('❶ Generate Github Token');
        printOut('⠀  Generate new personal access token with "repos scope". | size=12');
        printOut('❷ Create .prnotii File');
        printOut('⠀  Create ~/.prnotii File like below JSON format. | size=12');
        printOut('⠀  {"token": "kSEzyDP...", "endpoint": "https://api.github.com"}  | size=12');
        printOut('❸ Press Refresh button below');
        printOut('⠀  After finish ❶,❷ steps, press "Refresh" button to start again. | size=12');
      break;
      case e.message.includes('ETIMEDOUT') || e.message.includes('ECONNRESET'):
        printOut('🚧 CONNECTION ERROR');
        printOut('---');
        printOut('Check your internet connection or proxy settings.');
      break;
      default:
        printOut('⛑️ UNHANDLED ERROR');
        printOut('---');
        printOut(e.message + ' | size=12');
    }
  }

  printOut('---');
  printOut('🔄 Refresh | refresh=True');
  process.exit(0);
}) ();
