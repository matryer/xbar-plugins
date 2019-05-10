#!/usr/bin/env /usr/local/bin/node
// jshint asi:true
// <bitbar.title>GitLab Projects</bitbar.title>
// <bitbar.version>v2.0</bitbar.version>
// <bitbar.author>Shelton Koskie</bitbar.author>
// <bitbar.author.github>eightygrit</bitbar.author.github>
// <bitbar.desc>List of your last active GitLab projects. Now supports API v4</bitbar.desc>
// <bitbar.dependencies>node.js</bitbar.dependencies>
// <bitbar.image>http://i.imgur.com/4X40XIK.png</bitbar.image>

/**
 * Information
 *
 * This was inspired by the work of Sylvain Baronnet (@sylvainbaronnet), who made the
 * first version of the "GITLAB Projects" for API v3. What started as a couple of edits
 * became a total rewrite.
 *
 * @see   GitLab API Documentation    https://docs.gitlab.com/ee/api/README.html
 * @see   Create GitLab Access Token  https://gitlab.com/profile/personal_access_tokens
 * @see   BitBar Node Module Docs     https://github.com/sindresorhus/bitbar
 */



/**
 * The domain your instance is hosted on. Leave the default if using gitlab.com
 *
 * @var       {string}
 */
var gitlab_domain = 'gitlab.com';

/**
 * Your private access token.
 *
 * @var       {string}
 * @see       Create a token  https://gitlab.com/profile/personal_access_tokens
 */
var private_token = '';

/**
 * When you select a project, which sub-page should it take you to?
 *
 * @var  {string}
 *
 * Options include:
 * - /issues
 * - /activity
 * - /commits
 */
var default_page  = '/issues';

/**
 * How would you like results ordered?
 *
 * @var  {string}
 *
 * Options include:
 * - last_activity_at
 * - id
 * - name
 * - path
 * - created_at
 * - updated_at
 */
var order_by      = 'last_activity_at';

/**
 * Total number of results you want returned
 *
 * @var       {string}
 */
var result_count  = '30';

/**
 * Font size of the project list
 *
 * @var       {string}
 */
var font_size     = '15';

/////////////////////////////////////////////////////////////////////////
// Do not edit below this line unless you know what you're doing. :)  //
///////////////////////////////////////////////////////////////////////
var bitbar;

// Verify bitbar node module is available or try to install it globally.
try { bitbar = require('bitbar'); }
catch(e) {

  try { bitbar = globalRequire('bitbar'); }
  catch(e) {

    installBitbarModule();

    // Not catching error if one is thrown.
    bitbar = globalRequire('bitbar');
  }
}

/**
 * Performs the GET request for a projects list for authenticated user
 *
 * @param   {Function}  callback  The function to call to handle the response
 *
 * @return  {void}                Does not return anything
 */
(function(callback) {
    'use strict';

    const httpTransport = require('https');
    const responseEncoding = 'utf8';
    const httpOptions = {
        hostname: gitlab_domain,
        port: '443',
        path: '/api/v4/projects?order_by=' + order_by + '&sort=desc&archived=false&per_page=' + result_count + '&owned=true',
        method: 'GET',
        headers: {"PRIVATE-TOKEN":private_token}
    };
    httpOptions.headers['User-Agent'] = 'bitbar/gitlab_projects - node ' + process.version;

    const request = httpTransport.request(httpOptions, (res) => {
        let responseBufs = [];
        let responseStr = '';

        res.on('data', (chunk) => {
            if (Buffer.isBuffer(chunk)) {
                responseBufs.push(chunk);
            }
            else {
                responseStr = responseStr + chunk;
            }
        }).on('end', () => {
            responseStr = responseBufs.length > 0 ?
                Buffer.concat(responseBufs).toString(responseEncoding) : responseStr;

            callback(null, res.statusCode, res.headers, responseStr);
        });

    })
    .setTimeout(0)
    .on('error', (error) => {
        callback(error);
    });
    request.write("")
    request.end();


})((error, statusCode, headers, body) => {
    // console.log('ERROR:', error);
    // console.log('STATUS:', statusCode);
    // console.log('HEADERS:', JSON.stringify(headers));

    const bitbar = globalRequire('bitbar');
    var content = [];

    content.push({
      text: 'GitLab',
      color: bitbar.darkMode ? 'white' : 'black',
      dropdown: true
    });

    content.push(bitbar.separator);

    var projects = JSON.parse(body);

    for(var p in projects) {

      var project = projects[p];
      var ta = timeago();
      var last_activity = ta.ago(project.last_activity_at);

      content.push({
        text: project.name + ' ⤏ ' + 'Last activity: ' + last_activity + ' | href="' + project.web_url + '/activity' + '" size=' + font_size
      });

      content.push({
        text: project.name + ' ⤏ ' + project.open_issues_count + ' issue' + (project.open_issues_count > 1 ? 's' : '') + ' | alternate=true href="' + project.web_url + '/issues" size=' + font_size
      });

      // Uncomment if you want a separator after each project.
      //content.push(bitbar.separator);
    }

    // Execute the dispaly.
    bitbar(content);


});

/* Source : https://github.com/digplan/time-ago */
function timeago() {

  var o = {
    second: 1000,
    minute: 60 * 1000,
    hour: 60 * 1000 * 60,
    day: 24 * 60 * 1000 * 60,
    week: 7 * 24 * 60 * 1000 * 60,
    month: 30 * 24 * 60 * 1000 * 60,
    year: 365 * 24 * 60 * 1000 * 60
  };
  var obj = {};

  obj.ago = function(nd) {
    var r = Math.round,
      pl = function(v, n) {
        return n + ' ' + v + (n > 1 ? 's' : '') + ' ago'
      },
      ts = new Date().getTime() - new Date(nd).getTime(),
      ii;
      if(ts < 0) {
        return 'Just now';
      }

    for (var i in o) {
      if (r(ts) < o[i]) return pl(ii || 'm', r(ts / (o[ii] || 1)))
      ii = i;
    }
    return pl(i, r(ts / o[i]));
  }
  return obj;
}

/**
 * Sets up the ability to require global node packages.
 *
 * @return     {object}  Returns the required node package object
 */
function globalRequire(package){
  var childProcess = require('child_process');
  var path = require('path');
  var fs = require('fs');
  var env = Object.assign({}, process.env);
  env.PATH = path.resolve("/usr/local/bin") + ':' + env.PATH;


  var globalNodeModulesDir = childProcess.execSync(npmBin() + ' root -g', {env: env}).toString().trim() + '/';
  var packageDir = path.join(globalNodeModulesDir, package, '/');

  //find package required by older versions of npm
  if (!fs.existsSync(packageDir)){
    packageDir = path.join(globalNodeModulesDir, 'npm/node_modules/', package);
  }

  // Package not found
  if (!fs.existsSync(packageDir)){
    throw new Error('Cannot find global module \'' + package + '\'');
  }

  var packageMeta = JSON.parse(fs.readFileSync(path.join(packageDir, 'package.json')).toString());
  var main = path.join(packageDir, packageMeta.files[0]);

  return require(main);
}

/**
 * Installs Bitbar node module if it doesn't exits.
 *
 * @see    BitBar node module on github    https://github.com/sindresorhus/bitbar
 */
function installBitbarModule() {

    // Allows one to run the npm command as if on the command line.
    var childProcess = require('child_process');
    var execSync = childProcess.execSync;
    var path = require('path');
    var fs = require('fs');

    var env = Object.assign({}, process.env);
    env.PATH = path.resolve("/usr/local/bin") + ':' + env.PATH;

    // Get the path to npm bin
    var npm = npmBin();

    // The install command
    var cmd = npm + ' install -g bitbar';

    console.log("Installing the BitBar Node module...");

    var output = execSync(cmd, {
        cwd: process.cwd(),
        env: env
    }).toString('utf8').trim();

    console.log("Installation complete.");

}

/**
 * Gets the path to your npm executable.
 *
 * @return  {string}  The full path to your npm executable
 */
function npmBin(){
  var path = require('path');
  var childProcess = require('child_process');
  var execSync = childProcess.execSync;
  var env = Object.assign({}, process.env);
  env.PATH = path.resolve("/usr/local/bin") + ':' + env.PATH;
  var buffs = [];

  // Get the path to npm bin
  return execSync('which npm', {env: env}).toString('utf8').trim();
}