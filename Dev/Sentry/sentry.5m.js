#!/usr/bin/env /usr/local/bin/node

// <bitbar.title>Sentry</bitbar.title>
// <bitbar.version>v1.0</bitbar.version>
// <bitbar.author>Tommy Brunn</bitbar.author>
// <bitbar.author.github>nevon</bitbar.author.github>
// <bitbar.desc>Shows your most recent error reports from Sentry (https://getsentry.com)</bitbar.desc>
// <bitbar.dependencies>node.js</bitbar.dependencies>
// <bitbar.image>http://i.imgur.com/GdEXQfY.png</bitbar.image>

// jshint asi:true
var https = require('https')

/* EDIT HERE */
var AUTH_TOKEN = ''
var ORGANIZATION = ''
var PROJECT = ''
var ISSUE_COUNT = 5
/* DON'T EDIT BELOW */

var API_URL = 'https://' + AUTH_TOKEN + ':@app.getsentry.com/api/0/'
var PROJECT_URL = 'https://app.getsentry.com/' + ORGANIZATION + '/' + PROJECT
var TITLE = [ORGANIZATION + '/' + PROJECT, '@', 'Sentry'].join(' ')

function statusColor (issue) {
  var status = issue.status
  var isAssigned = issue.assignedTo !== null
  if (status === 'resolved' || status === 'muted') {
    return 'green'
  }
  if (status === 'unresolved' && isAssigned) {
    return 'orange'
  }

  return 'red'
}

function timeSince (date) {
  if (typeof date !== 'object') {
    date = new Date(date)
  }

  var seconds = Math.floor((new Date() - date) / 1000)
  var intervalType

  var interval = Math.floor(seconds / 31536000)
  if (interval >= 1) {
    intervalType = 'year'
  } else {
    interval = Math.floor(seconds / 2592000)
    if (interval >= 1) {
      intervalType = 'month'
    } else {
      interval = Math.floor(seconds / 86400)
      if (interval >= 1) {
        intervalType = 'day'
      } else {
        interval = Math.floor(seconds / 3600)
        if (interval >= 1) {
          intervalType = 'hour'
        } else {
          interval = Math.floor(seconds / 60)
          if (interval >= 1) {
            intervalType = 'minute'
          } else {
            interval = seconds
            intervalType = 'second'
          }
        }
      }
    }
  }

  if (interval > 1 || interval === 0) {
    intervalType += 's'
  }

  return interval + ' ' + intervalType
}

function trimString (str, n) {
  n = n || 72
  return (str.length > n) ? str.substr(0, n - 1) + '…' : str
}

function formatTitle (issue) {
  return [trimString(issue.title), ' | href=', issue.permalink, ' size=11', ' color=' + statusColor(issue)].join('')
}

function formatCulprit (issue) {
  return [trimString(issue.culprit), '| size=10'].join('')
}

function formatCount (count) {
  var str = (count === 1) ? 'occurrence' : 'occurrences'
  return [count, str, '| size=10'].join(' ')
}

function formatTimes (issue) {
  var lastSeen = timeSince(new Date(issue.lastSeen)) + ' ago'
  var firstSeen = timeSince(new Date(issue.firstSeen)) + ' old'
  return [lastSeen, '-', firstSeen, '| size=10'].join(' ')
}

function formatIssue (issue) {
  return [
    formatTitle(issue),
    formatCulprit(issue),
    formatTimes(issue),
    formatCount(issue.count)
  ].join('\n')
}

function handleResponse (body) {
  var output = body.map(formatIssue).join('\n---\n')
  console.log('Sentry' + '\n---\n' + TITLE + ' | href=' + PROJECT_URL + '\n---\n' + output)
}

https.get(API_URL + 'projects/' + ORGANIZATION + '/' + PROJECT + '/issues/?query=is%3Aunresolved&limit=' + ISSUE_COUNT + '&sort=date&statsPeriod=24h', function (res) {
  var body = ''
  res.on('data', function (data) {
    body += data
  })
  res.on('end', function () {
    handleResponse(JSON.parse(body))
  })
})
