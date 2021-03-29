#!/usr/bin/env /usr/local/bin/node
/* jshint esversion: 8 */
/* jshint asi: true */

// <xbar.title>Slack Team Notifications</xbar.title>
// <xbar.version>v1.1.1</xbar.version>
// <xbar.author>Benji Encalada Mora</xbar.author>
// <xbar.author.github>benjifs</xbar.author.github>
// <xbar.image>https://i.imgur.com/ORbsRBx.jpg</xbar.image>
// <xbar.desc>Show notifications for Slack teams and channels with option to mark as read. See https://github.com/benjifs/bitbar-slack-team-notifications for configuration instructions.</xbar.desc>
// <xbar.dependencies>node.js superagent</xbar.dependencies>

const request = require('superagent');
const tokens = require('./.tokens.js');

// CONFIG
// BITBAR_SLACK_ICON defines which Slack Icon is shown
// 0 -> Original Slack Icon
// 1 -> White Slack Icon
// 2 -> Black Slack Icon
const BITBAR_SLACK_ICON = process.env.BitBarDarkMode ? 1 : 2;
// If MENTIONS_ONLY is true, the count only includes mentions and DMs.
// If MENTIONS_ONLY is false, the count includes all unread messages.
const MENTIONS_ONLY = false;
// MAX_LENGTH of channel name or user name
const MAX_LENGTH = 18;
// Show `No new notifications` message
const SHOW_ZERO_NOTIFICATIONS = true;

// Is Slack.app installed?
let SLACK_INSTALLED = true;
const { exec } = require('child_process');
exec('ls /Applications | grep Slack | wc -l', (err, stdout, stderr) => {
	if (!err && stdout == 0) {
		SLACK_INSTALLED = false;
	}
});

const DEBUG = process.argv.indexOf('--debug') > 0;
const SCRIPT = process.argv[1];

// Slack App Credentials
const SLACK_OAUTH_AUTHORIZE = 'https://slack.com/oauth/authorize?scope=client&client_id=';
const SLACK_CLIENT_ID = '11708641376.684689498789';

// Slack API
const SLACK_API = 'https://slack.com/api/';
const SLACK_CONVERSATIONS = 'conversations';
const SLACK_CHANNELS = 'channels';
const SLACK_GROUPS = 'groups';
const SLACK_IM = 'im';
const SLACK_TEAM = 'team';
const SLACK_USERS = 'users';
const SLACK_INFO = '.info';
const SLACK_LIST = '.list';
const SLACK_MARK = '.mark';
const SLACK_HISTORY = '.history';
const SLACK_AUTH_TEST = 'auth.test';

// ICONS {
// Original Slack icon (unused)
const SLACK_ICON = 'image=iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAACatJREFUWAmlWGtsVMcVPjP33t21vbt+Ydog8ZSSNjXhISgRkFLTH3kBoSQxceBPo/5o+6tNq/KjvNaISBVSkfqvitTSRIUATtQmMa/8CKaNjSCkBIOVhKaBEBEpNn7trr3rvXdm+p25u8bYsFjqkWZ27syZc86cty3ofmCMICEMo63oHNxNJLZimcS4opXefWFNXWfKGJlq2yxoc5ta2L59lZRyD84XYqTJiEPd6/aksCZKpSSGtut7TOIe+7e3iwJ9v3PojciM6pZgOEekFMnKOOncCOmC3/ThmtozfOGRY9vXSMc5IyujpEcLRI4gN1lJfn/6SPfTe1vIEB6HuQzIMmfUbIzD2lnRNfyEW5Vs8W+lfVMoKKOVUZnhvKyoJOmIfSUaQjj7ZEWEVDafZxxTCJQ/kPGdqtgLi0/seoKFaTqdckv4d/ste9jXARIAo/Wq8cuCHLsWFNWjI3iu+W7TRVMzdJ2fLh5WOWiGTBSmZW0wruYlGfMo1qcyV78ua5WyGrKMMYFyliTTMbfVbflbDAnrOHlDDrwNWJP4sZn4psApIPHQrNs0eGMSlBWoqQmvYzD+2zCREV4kgi9WQQAOAZsFa+XEy/sFcKYNZQVKCaGbjxrn/A8arkI5L7Ezu8nqiBOvdp14IuJWVwvhetVuf9GMHAD/J4z7UNPp024mkRCJzLI7VNoHBo09JnK+Uby2vGuw26SHW7BVB50o5m2kyTmzKRPJbUpi5467fH5PONrsLBustQphM3asTQWM69rcsHu36RDCbtyTAA4urKq9iB8eU2Dxu3vryRmDicsriZ26GcK0IWd9BHOPE2Jfa00JFkhj0KOdQ49jbyXe7MEx4b1T1W+d1hBeJQL49MVzq2veKhGU0bRRfoTjqSzka2eJts2vqiXv7XhQB2Ij2MSFpLOXxJ5TyJzGmoyTnkSeuUuM3JW4jTVwXtE5dEpFR5/9aPmsURXUSyGy9zVZz+ZU4ZETO3+iAzrgxGN4f8hi0fEdNnm6KzqHU94MJL2+4QD5AsdTNXNXqZBfIg3VTxR69X6c/9zzYrLgIzvcAxnVxeavxcd3LiUpD/DjkUA5YiWMZbz65AuLju36FPfNVlsOrPGNBwTW2nSG4w+M4JbctOyC8XzdkYepixYLa99E2XBQkrWFS4vx1RjOOY240JII0qOgZbYwEqIDvoWkNpHANNbSGE5TJoZqETf+3/2i7/Hena4EvUP51jiY60jbZZjxLSOUKJvSKMlCXOFCCWCJGXNaA4QLTqySsa99sEgMXl5Pg9roazLKSubkaQXgKELpwJcWH/MBgqUUWXgN4/AQY7KSlQVZjJA71UiGnKpkDLYVAtXyfoPx3Mo49M605HamZEGa3/GerIhGLS1HOl59IqIy+ZNoQcYjUrjWGB7zYTynKhrTo2Oktd4lP1yV7Ap0sEZn0+dcpdKuCrJ2BMGIxBrlIT1xSK0yjKdHM5cKudH151dXH4MQkkxKXn5q73E1VlhHOf/jiDbpiNK9Xn/mT8/qb54ryRwXIudk83Aek/aMzjKeyebPF5T64ZX1r3QJQmmgzSJU46CpoZFiGWAKeXJ+1Nvv3dQVptHPy3gwJl//9gNj8LoCzRWDlgkLgxITrlNYI68xnNxfRzPWZGj5ct9+l6bTB2KUyyQon9WURJ/n+IrWvjxkj5EwORy4jlsiWNfggJ0NS/ubwVneIhcnHLCTxLFvBTKUwv1QCJMCrVSRVjvVQmzv66o10WjB1xXeEKwSEzPUxYAqUZxH2bMAiH6xiaxAbG27+cXsLej0xD7jiIfxWhiWfJdkje+YP87/7PVf8b2ul/dXPHD80h9I6edgohii4pr0acfcmwfbWSiYjINeD7QvXKe08wquLGDiUJ5BzQ2Z4xsRbrexZMD7hEYa+tSQ3tbwzOUz4ss5W1fDxT6olC6NIn0yoEaaehkTt3T+1fk3Dv2M967P3nriW27syX6kDw06FcBnyiM62DD3xsF2xhloX7IOKaCdWeZsWRuXw+Ky2qcA1FJV4dBoXtNYIB6TCMM9VdKjjPbzLAgPXPJ9q/kwwX0xp+X5pOM9+Q3awYDQ2gNnBPiMaKTeW2ICVexlzpmcHmMkxc1ucQToaEtrtFHj+4w3PBLkq2ISLbhuhUC0EMSZJtpO+xB+VlGvcHiAFHIpBAd3OHDoW8Ax0Rxr1BHzu+kXtcNdVIc8uYA1g0NOKkyH8Utj/BuUSnv8C14ims1ZN17ITpRG5sF+6Iy8AMCu3KOaAf5AknX5O+TBOwzFnlQbWUlZL7mSMmCURybjszDSLN60Ju2EeTvNAv2tRrJy2DUMO1EBGSua1QWIbg6H5CQEZADLyaCErnjcj0IOLh1vVVdzGbTZmGndd9h7CIZEAiXN0EF37o1Drdfnbv1OXHovhjoQlIYwsOVL824cts2YErZogX4oFstUWkFEoTPwBEDuvxW/uSVG5tVUOU+x6BYHUwmXcUpgn1Z8H3vtwKB/uGFjdys/h+Z9eXDLtdlbXoO6VqIsZpVS78y7eeRqT2NzpLGnrcCGtjCBMswZssKs/hO1As3+9Vn8FUlP9/5j0fNS0tLwjg3rUv0K6fDMN0LCiHg6V7+x+yRvuyXC4qtD6NiIhwVDzQ719N3pC+PP5lfbCGRHI1mPBvJW6SbRzB93v4kvHtMGaMk+l53VKs4KQH1FHTTBZ1JoCJqsBstRRdslCp95AQjG+t9Z8nshFNpAVgASrBADePCRhg3d/+452hj5XkPDnQ8sEu44gweKDvY324jZbUFtE9TaYfemThNUVDyE4USUIkH2IiU8l36ZjHsUBGE+5oBDOG/rfXvxT2duvPQXw7UKzf1Uurd3xt3j9tbdVqEWJ55YtWIDTE0WAaajpPzADPUPB0h0qjCUVcFgRo2xyqUwf+59d8mDLMzRoxOK90SCxfU0BcIfwvZC0aKTCCUogbaIfcC4TpiIuACzuaOFwBSqq2BXbZ7hawsGl5XleV8fYSKQxrcuZ12Yd9ipw6wE30GeH1GKewKupJNyFTRoHyOlsW0p3y0HZaUlarCKAcWzyJolOuwDnDoKVSiwaB4+mU9/HapdihbCmE+qYjblltphVDAIFFI5ywQyDyXGCZUITvwtKxA7Om6LOUgJWRO8MdOp8CLoO/mfRkkZiXF3oLXZViII9fx2JKcIJoqx6SKucOrrPDebU4frN1x6j4VbuzaMptKdyb9lBQqRwwdx8uxTuVTB6M/REfQim7+PGH5swVeH/sn9EDsr8s+/jNKr4dTvK2160Rp8jgzcWr+h+0Wm1doa5prJQkz8/h9TqIg86VwbWQAAAABJRU5ErkJggg==';

// Dark Slack Icon
const SLACK_ICON_B = 'image=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';

// White Slack Icon
const SLACK_ICON_W = 'image=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'
// }

let unread_count = 0;
const slack_output = {};
const errors = [];
let call_log = {};

debug('Debugging');

if (process.argv.indexOf('--mark') > 0) {
	console.log('Mark as read');

	let token;
	for (let i = 3; i < process.argv.length; i++) {
		if (process.argv[i].indexOf('--token=') === 0) {
			token = process.argv[i].split('=')[1];
		}
	}
	if (!token) {
		console.log('Error: Missing token');
		return;
	}

	for (let i = 3; i < process.argv.length; i++) {
		let args = process.argv[i].split('=');
		if (args.length != 2) {
			continue;
		}
		if ([SLACK_CHANNELS, SLACK_GROUPS, SLACK_IM].indexOf(args[0]) < 0) {
			continue;
		}

		let channels = args[1].split(',');
		for (let j in channels) {
			console.log('/' + args[0] + SLACK_MARK + ' (' + channels[j] + ')');
			slack_request(SLACK_CONVERSATIONS + SLACK_MARK, token, {
				'channel': channels[j],
				'ts': Math.floor(Date.now() / 1000) + '.000000'
			})
				.then((body) => {
					// console.log('  Success: ' + args[0] + ':' + channels[j]);
				});
		}
	}
	return;
}

function debug(message) {
	return DEBUG && console.log(message);
}

function slack_request(URL, token, query) {
	debug('  /' + URL + (query && query.channel ? ' (' + query.channel + ')' : ''));
	// The following is to keep track of how many calls are being made
	// for each token to each method. Should help debug the rate limits
	if (!call_log[token]) {
		call_log[token] = {};
	}
	if (!call_log[token][URL]) {
		call_log[token][URL] = 0;
	}
	call_log[token][URL]++;
	return request
		.get(SLACK_API + URL)
		.query(query)
		.auth(token, { type: 'bearer' })
		.then((res) => {
			debug(res.body);
			if (res && res.body && res.body.ok === true) {
				return Promise.resolve(res.body);
			}
			return Promise.reject(res.body.error);
		})
		.catch((err) => {
			debug('ERROR: ' + err);
			debug('  ' + URL);
			debug('  ' + JSON.stringify(query));
			errors.push(URL + ': ' + err + ' | color=red');
		});
}

function output() {
	unread_count = unread_count > 10 ? '10+' : unread_count > 0 ? unread_count : '';
	const slack_icon = BITBAR_SLACK_ICON == 2 ? SLACK_ICON_B : BITBAR_SLACK_ICON == 1 ? SLACK_ICON_W : SLACK_ICON;
	if (errors.length > 0) {
		console.log('! |color=red ' + slack_icon);
	} else {
		console.log(unread_count + ' | ' + slack_icon);
	}

	if (Object.keys(slack_output).length) {
		for (let i in slack_output) {
			let team = slack_output[i];

			if (SHOW_ZERO_NOTIFICATIONS || team.notifications.count > 0) {
				console.log('---');
				console.log(team.name + ' | size=12');
			}
			if (team.notifications.count > 0) {
				if (team.notifications.channels.length > 0) {
					console.log('Channels | size=12');
					for (let j in team.notifications.channels) {
						console.log(team.notifications.channels[j]);
					}
				}
				if (team.notifications.dms.length > 0) {
					console.log('Direct messages | size=12');
					for (let j in team.notifications.dms) {
						console.log(team.notifications.dms[j]);
					}
				}
				console.log('Mark all as read ' +
					'|bash=' + SCRIPT +
					' param1=--mark' +
					' param2=--token=' + team.token +
					(team.params[SLACK_IM] ? ' param3=' + SLACK_IM + '=' + team.params[SLACK_IM].join() : '') +
					(team.params[SLACK_GROUPS] ? ' param4=' + SLACK_GROUPS + '=' + team.params[SLACK_GROUPS].join() : '') +
					(team.params[SLACK_CHANNELS] ? ' param5=' + SLACK_CHANNELS + '=' + team.params[SLACK_CHANNELS].join() : '') +
					' refresh=true' +
					' terminal=false');
			} else if (SHOW_ZERO_NOTIFICATIONS) {
				console.log('No new notifications');
			}
		}
	}
	if (errors.length > 0) {
		console.log('---');
		console.log('Errors');
		for (let i in errors) {
			console.log('--' + errors[i]);
		}
	}
	console.log('---');
	console.log('Add New Workspace');
	console.log('--Generate OAuth Token | href=' + SLACK_OAUTH_AUTHORIZE + SLACK_CLIENT_ID);
	debug(call_log);
}

function format_channel_name(channel) {
	let name = '';
	if (channel.is_im) {
		name = '@' + channel.name;
	} else if (channel.name.indexOf('mpdm-') === 0) {
		let users = channel.name.split('-');
		users = users.filter((user, i) => {
			// mpdm string is of this format: `mpdm-xxxxx--yyyyy--zzzzz-n`
			// where xxxxx, yyyyy, and zzzzz are usernames
			// Remove empty strings, first, and last elements of the array
			return user !== '' && i !== 0 && i !== users.length - 1;
		});
		// Using │ since bitbar can not display the standard | character
		name = '@' + users.join('│@');
	} else {
		name = '#' + channel.name;
	}
	if (name.length > MAX_LENGTH) {
		name = name.substring(0, MAX_LENGTH - 1) + '…';
	}
	return name;
}

function channel_output(channel) {
	unread_count += channel.count;

	let output_str = format_channel_name(channel);
	output_str += ' '.repeat(MAX_LENGTH + 2 - output_str.length);
	output_str += (channel.count > 10 ? '10+' : channel.count);

	let key = channel.is_im ? SLACK_IM : channel.is_channel ? SLACK_CHANNELS : SLACK_GROUPS;
	let href;
	if (SLACK_INSTALLED) {
		href = 'slack://channel?team=' + channel.team + '&id=' + channel.id;
	} else {
		href = 'https://app.slack.com/client/' + channel.team + '/' + channel.id;
	}

	output_str += '|font=Menlo size=13 href=' + href;
	let alt_str = 'Mark as read ' +
		'|alternate=true' +
		' font=Menlo size=13' +
		' bash=' + SCRIPT +
		' param1=--mark' +
		' param2=--token=' + channel.token +
		' param3=' + key + '=' + channel.id +
		' refresh=true' +
		' terminal=false';

	if (output_str[0] == '@') {
		slack_output[channel.token].notifications.dms.push(output_str);
		slack_output[channel.token].notifications.dms.push(alt_str);
	} else {
		slack_output[channel.token].notifications.channels.push(output_str);
		slack_output[channel.token].notifications.channels.push(alt_str);
	}
	slack_output[channel.token].notifications.count++;

	if (!slack_output[channel.token].params[key]) {
		slack_output[channel.token].params[key] = [];
	}
	slack_output[channel.token].params[key].push(channel.id);
}

async function run() {
	if (typeof tokens === 'undefined' || !tokens || !tokens.length) {
		errors.push('Missing Slack Legacy Token | color=red href=https://api.slack.com/custom-integrations/legacy-tokens');
		errors.push('Generate OAuth Token | color=red href=' + SLACK_OAUTH_AUTHORIZE + SLACK_CLIENT_ID);
		return output();
	}

	for (let i in tokens) {
		await get_team_notifications(tokens[i]);
	}
	output();
}

function get_team_notifications(token) {
	return get_team_info(token)
		.then((team) => {
			if (team) {
				slack_output[token] = {
					'id': team.id,
					'name': team.name,
					'token': token,
					'notifications': {
						'channels': [],
						'groups': [],
						'dms': [],
						'count': 0
					},
					'params': {},
					'errors': []
				};
				return get_auth_info(token);
			} else {
				throw 'Invalid token';
			}
		})
		.then((user_id) => {
			slack_output[token].user_id = user_id;
			return get_team_conversations(token);
		})
		.then((channels) => {
			return check_channels_unread(channels, token);
		})
		.then((channels) => {
			for (let i in channels) {
				if (channels[i]) {
					channel_output(channels[i]);
				}
			}
		})
		.catch((errors) => {
			debug(errors);
		})
}

function get_team_info(token) {
	debug('Fetching team info for ' + token);
	return slack_request(SLACK_TEAM + SLACK_INFO, token)
		.then((body) => {
			if (body && body.team) {
				return Promise.resolve(body.team);
			}
		});
}

function get_auth_info(token) {
	debug('Fetch auth info for ' + token);
	return slack_request(SLACK_AUTH_TEST, token)
		.then((body) => {
			if (body && body.user_id) {
				return Promise.resolve(body.user_id);
			}
		});
}

function get_team_conversations(token) {
	debug('Fetching conversations for ' + token);
	return slack_request(SLACK_CONVERSATIONS + SLACK_LIST, token, {
		'exclude_archived': true,
		'limit': 200,
		'types': 'public_channel,private_channel,mpim,im'
	})
		.then((body) => {
			if (body && body.channels) {
				return Promise.resolve(body.channels);
			}
		});
}

async function check_channels_unread(channels, token) {
	let req = [];
	for (let i in channels) {
		if (channels[i]) {
			req.push(is_channel_unread(channels[i], token));
		}
	}
	return await Promise.all(req);
}

function is_channel_unread(channel, token) {
	return get_unread_count(channel, token)
		.then((unread_count) => {
			if (channel && unread_count > 0) {
				if (channel.is_im) {
					return get_user(channel.user, token)
						.then((user) => {
							if (user) {
								return Promise.resolve({
									'id': channel.id,
									'name': user.name,
									'count': unread_count,
									'team': user.team_id,
									'is_im': true,
									'token': token
								});
							}
						});
				} else if (channel.is_member || channel.is_group) {
					let team = channel.shared_team_ids && channel.shared_team_ids.length > 0 ? channel.shared_team_ids[0] : '';
					return Promise.resolve({
						'id': channel.id,
						'name': channel.name,
						'count': unread_count,
						'team': team,
						'is_channel': channel.is_member && channel.is_channel,
						'is_group': channel.is_group || (channel.is_channel && channel.is_private),
						'token': token
					});
				}
			}
		});
}

function get_unread_count(channel, token) {
	// unread_count_display is a count of messages that the calling user has
	// yet to read that matter to them (this means it excludes things like
	// join/leave messages)
	// unread_count_display does not show up consistently.
	// In the case unread_count_display is not present, check conversations.history
	if (channel && !('unread_count_display' in channel)) {
		return check_conversation_history(channel, token)
			.then((unread_count) => {
				return Promise.resolve(unread_count);
			})
	} else {
		return Promise.resolve(channel.unread_count_display);
	}
}

function check_conversation_history(channel, token) {
	debug('Fetch history for ' + channel.id);
	return slack_request(SLACK_CONVERSATIONS + SLACK_HISTORY, token, {
		'channel': channel.id,
		'oldest': channel.last_read != '0000000000.000000' ? channel.last_read : 0,
		'unreads': true
	})
		.then((body) => {
			if (body && body.unread_count_display > 0) {
				const user_id = slack_output[token].user_id;
				if (channel.is_im || channel.is_mpim) {
					return Promise.resolve(body.unread_count_display);
				}
				return Promise.resolve(count_mentions(body, user_id));
			}
			return Promise.resolve(0);
		});
}

function count_mentions(body, user_id) {
	if (MENTIONS_ONLY && body.messages && body.messages.length > 0) {
		let count = 0;
		for (let i in body.messages) {
			if (i >= body.unread_count_display) {
				break;
			}
			if (body.messages[i].text.indexOf(user_id) >= 0) {
				count++;
			}
		}
		return count;
	}
	return body.unread_count_display;
}

function get_user(user, token) {
	debug('Fetch user info for ' + user);
	return slack_request(SLACK_USERS + SLACK_INFO, token, {
		'user': user
	})
		.then((body) => {
			if (body && body.user) {
				return Promise.resolve(body.user);
			}
		});
}

run();

