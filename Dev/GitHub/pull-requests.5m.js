#!/usr/bin/env

// <bitbar.title>Github Pull requests</bitbar.title>
// <bitbar.version>v2.0</bitbar.version>
// <bitbar.author>Noam Knispel</bitbar.author>
// <bitbar.author.github>noamknispel</bitbar.author.github>
// <bitbar.desc>Get list of pull requests from Github for multiple repositories</bitbar.desc>
// <bitbar.dependencies>node.js request co bluebird</bitbar.dependencies>

// EDIT YOUR INFO BELOW:
const username = ''
const token = ''
var repos = [
//owner/repo
]
// DO NOT EDIT BELOW THIS POINT

var bluebird = require("bluebird")
var request = bluebird.promisifyAll(require("request"))
var co = require("co")

co(function* () {
  var results = yield repos.map(function(repo) {
    var options = {
      url: 'https://api.github.com/repos/' + repo + '/pulls',
      headers: {
        'User-Agent': username + ' - bitbar'
      },
      auth: {
        user: username,
        password: token
      }
    }
    return request.getAsync(options);
  })

  var totalCount = 0
  var strings = results.reduce( (acc, response) => {
    var info = JSON.parse(response.body)
    if(info.length > 0) {
      totalCount += info.length
      acc.push(info[0].base.repo.name + " | color=#0000ff")
      info.forEach(function(pr) {
        acc.push("#" + pr.number + " " + pr.title + " (" + pr.user.login + ") | href=" + pr.html_url)
      })
    }
    return acc
  }, ['---'] )

  strings.unshift(`${totalCount} Pull Request`)
  for( var string of strings)
    console.log(string)
})
