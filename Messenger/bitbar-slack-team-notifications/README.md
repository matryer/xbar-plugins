# Slack Team Notifications
[Bitbar](https://github.com/matryer/bitbar) plugin to show the notification count from multiple [Slack](https://slack.com) teams and channels. You can also mark channels or whole teams as read.

**The original repo can be found here:** https://github.com/benjifs/bitbar-slack-team-notifications

![Slack Team Notifications screenshot](https://i.imgur.com/x1SoIto.jpg)

## Install
1. Download and install [Bitbar](https://github.com/matryer/bitbar). You can also install it with [Homebrew](https://brew.sh/) by running `brew cask install bitbar`

2. Clone this repo to a directory of your choice. You will symlink the executable file afterwards to your bitbar plugins folder

3. Inside the cloned directory, run `npm install`

4. Navigate to your bitbar plugins folder and symlink `slack-team-notifications.1m.js`

```
ln -s {CLONED_REPO_DIRECTORY}/slack-team-notifications.1m.js slack-team-notifications.1m.js
```

## Configure
You will need to get your [Slack Legacy Tokens](https://api.slack.com/custom-integrations/legacy-tokens) for every team you want to get notifications from. After getting the legacy tokens, add them to the `tokens.js` file:

```
module.exports = [
	'xoxp-123456',
	'xoxp-abcdef'
];
```

## Issues
For any issues or suggestions, the original repo can be found here:

https://github.com/benjifs/bitbar-slack-team-notifications

