#!/usr/bin/env /usr/local/bin/node

// <bitbar.title>Github Repository Issues</bitbar.title>
// <bitbar.version>v1.0</bitbar.version>
// <bitbar.author>Tom Genoni</bitbar.author>
// <bitbar.desc>Lists all pull requests and issues, up to 100 total.</bitbar.desc>
// <bitbar.author.github>tomgenoni</bitbar.author.github>
// <bitbar.dependencies>node.js</bitbar.dependencies>

$HOSTNAME = ""; // For enterprise Github accounts, e.g., github.tinderforcats.com/api/v3
$ORG = "matryer";
$REPOSITORY = "bitbar";
$BITBAR_TITLE = "bitbar";
$GITHUB_ACCESS_TOKEN = "";

// ----------- Ignore below this line ---------------

var https = require('https');

var options = {
  host: $HOSTNAME || 'api.github.com',
  headers: {'user-agent': 'Mozilla/5.0'},
  path: '/repos/'+$ORG+'/'+$REPOSITORY+'/issues?GITHUB_ACCESS_TOKEN=' + $GITHUB_ACCESS_TOKEN + '&per_page=100'
};

https.get(options, function(res) {
  var body = '';
  res.on('data', function(data) {
    body += data;
  });
  res.on('end', function() {
    handleResponse(JSON.parse(body));
  });
});

function fixedIssues(body) {

  var arr = [];

  var closeTerms = ['close', 'closes', 'closed', 'fix', 'fixes', 'fixed', 'resolve', 'resolves', 'resolved'];

  body.map(function(issues){
    if ( issues.pull_request ) {
      var comments = issues.body;

      closeTerms.forEach(function(term) {
        var regex = new RegExp(term + " #([0-9]+)", "ig");
        var matches = comments.match(regex);
        if (matches) {
          matches.forEach(function(match) {
            var num = match.split("#")[1];
            arr.push(num);
          });
        }
      });
    }
  });

  return arr;
}

function allIssues(body) {

  var fixed = fixedIssues(body);

  var issues = body.map(function(issues){
    if ( issues.pull_request ) {
      return [issues.title, ' - #', issues.number, ' | href=', issues.html_url,'\n'].join('');
    }
  }).join('\n');

  var prs = body.map(function(issues){
    var color = "";
    if ( !issues.pull_request ) {
      if ( fixed.indexOf(issues.number.toString()) > -1 ) {
        color = "color=#aaaaaa";
      }
      return [issues.title, ' - #', issues.number,' (', issues.user.login, ') | href=', issues.html_url, ' ', color, '\n'].join('');
    }
  }).join('\n');

  return [issues, prs];
}

function issueCount(body) {
  var p = 0;
  body.map(function(issues){
    if ( issues.pull_request ) {
      p++;
    }
  });
  var i = body.length - p;
  return [i, p];
}

function handleResponse(body) {
  var countPRs = issueCount(body)[1];
  var countIssues = issueCount(body)[0];
  var listIssues = allIssues(body)[1];
  var listPRs = allIssues(body)[0];

  console.log($BITBAR_TITLE + " " + countPRs + "/" + countIssues);

  console.log("---");
  console.log("Pull Requests " + countPRs);
  console.log(listPRs);

  console.log("---");
  console.log("Issues " + countIssues);
  console.log(listIssues);

}
