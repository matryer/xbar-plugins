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
You will need to get your [Slack Legacy Tokens](https://api.slack.com/custom-integrations/legacy-tokens) for every team you want to get notifications from. After getting the legacy tokens, add them to the `.tokens.js` file:

```
module.exports = [
	'xoxp-123456',
	'xoxp-abcdef'
];
```

If you cannot use Slack Legacy Tokens, you can generate a token by using [OAuth](https://api.slack.com/docs/oauth). In order to use OAuth to generate a token it is **recommended** you create your own [Slack app](https://api.slack.com). There is a sample app created to show what the process would be like but ideally, you would create your own app. The app will need to have the following permissions:

|Permission|Reason|
|---|---|
|`channels:read`|Check channel for unread messages|
|`channels:write`|Mark channel as read|
|`groups:read`|Check groups for unread messages|
|`groups:write`|Mark group as read|
|`im:read`|Check IM for unread messages|
|`im:write`|Mark IM as read|
|`mpim:read`|Check mpim for unread messages|
|`mpim:write`|Mark mpim as read|
|`users:read`|If unread IM, get username for user|
|`team:read`|Get team name|

The app should also have a valid redirect URL that will received `code` as a GET parameter and then makes a call to Slack's [oauth.access](https://api.slack.com/methods/oauth.access) method to get the final token. You can see a basic sample of how this is handled in the [index.html](https://github.com/benjifs/bitbar-slack-team-notifications/blob/master/index.html) for this plugin.

## Issues
For any issues or suggestions:
https://github.com/benjifs/bitbar-slack-team-notifications/issues

