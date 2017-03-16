#!/usr/local/bin/node
/* jshint esversion: 6 */

/*
# <bitbar.title>GitBar</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>darkturtle</bitbar.author>
# <bitbar.author.github>clthck</bitbar.author.github>
# <bitbar.desc>Quickly check your GitHub stats</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/clthck/gitbar/master/Screenshot.png</bitbar.image>
# <bitbar.dependencies>node,github-contrib-stats,dotenv</bitbar.dependencies>
# <bitbar.abouturl>https:/github.com/clthck/gitbar</bitbar.abouturl>
*/

require('dotenv').config({ path: __dirname + '/../.env' });
const child_process = require('child_process');
const getGitHubContribStats = require('github-contrib-stats');

// Reads user settings
const username = process.env.GITHUB_USERNAME;
const userUrl = `http://github.com/${username}`;
const contributionGoalTracking = process.env.CONTRIBUTION_GOAL_TRACKING;
const contributionGoal = process.env.CONTRIBUTION_GOAL;
const compactUI = process.env.COMPACT_UI;

// Detects user's menu bar style (assumes dark menu bar)
let boldColor = 'white';
try {
  child_process.execSync('defaults read -g AppleInterfaceStyle', { stdio: 'ignore' });
} catch (err) {
  // AppleInterfaceStyle not set, which means user has light menu bar style
  boldColor = 'black';
}

// Font, Color, and Emoji Settings
const redText = '| color=red size=14';
const normalText = '| size=14';
const boldText = `| color=${boldColor} size=14`;
const heartEmoji = '♥︎';
const brokenHeartEmoji = `♡`;

// Makes sure user overrided default configs
if (username === '<YOUR_GITHUB_NAME_HERE>') {
  console.log(brokenHeartEmoji, 'GH username not set!', brokenHeartEmoji);
  process.exit();
}

getGitHubContribStats(username)
  .then(data => {
    const { totalContributions, todaysContributions, currentStreak, longestStreak }  = data.contributionStats;
    const { name, followers }  = data.basicUserData;
    const visibleEmoji = totalContributions ? heartEmoji : brokenHeartEmoji;
    // text colors
    const contributionsTodayColor = todaysContributions ? normalText : redText;
    const currentStreakColor = currentStreak ? normalText : redText;
    const totalContributionsColor = totalContributions ? normalText : redText;

    // Logs output to Bitbar
    if (compactUI == 'true') {
      console.log(visibleEmoji + ' ' + todaysContributions + contributionsTodayColor);
      console.log('---');
      console.log('Contributions');
      console.log('Today: ', todaysContributions, contributionsTodayColor);
    } else {
      console.log(visibleEmoji, ' Contributions Today: ', todaysContributions, visibleEmoji, contributionsTodayColor);
      console.log('---');
    }

    console.log('Total: ', totalContributions, totalContributionsColor);

    if (contributionGoalTracking) {
      console.log('---');
      console.log('Contribution Goal');
      console.log('Goal: ', contributionGoal, normalText);
      console.log('Completion: ', `${(totalContributions / contributionGoal * 100).toFixed(2)}% ${boldText}`);
    }

    console.log('---');
    console.log('Streaks');
    console.log('Current: ', currentStreak, currentStreakColor);
    console.log('Longest: ', longestStreak, normalText);
    console.log('---');

    console.log(`${name} (a.k.a @${username})`, `| size=14 href=${userUrl}`, 'image=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');

    console.log('---');
    console.log(`${followers} people are following you`, normalText, `href=${userUrl}/followers`);
  })
  .catch(err => {
    console.log(`${brokenHeartEmoji} error`, redText);
  });
