#!/usr/bin/env /usr/local/bin/node

// <bitbar.title>Toggl status</bitbar.title>
// <bitbar.version>v1.1</bitbar.version>
// <bitbar.author>Stefan du Fresne</bitbar.author>
// <bitbar.author.github>SCdF</bitbar.author.github>
// <bitbar.desc>Shows hours completed today, hours completed this week.</bitbar.desc>
// <bitbar.image>https://i.imgur.com/1dlNZLW.png</bitbar.image>
// <bitbar.dependencies>node</bitbar.dependencies>

/*jshint esversion: 6 */

const fs = require('fs');

// TODO: Add month-long work leveling
//   You're supposed to work N hours a week, but also N*M hours a month
//   Take longer / shorter weeks into account all the way to the month

/* jshint -W100 */
const AVATARS = {
  '👶': ['👶', '👶🏻', '👶🏼', '👶🏽', '👶🏾', '👶🏿'],
  '👦': ['👦', '👦🏻', '👦🏼', '👦🏽', '👦🏾', '👦🏿'],
  '👧': ['👧', '👧🏻', '👧🏼', '👧🏽', '👧🏾', '👧🏿'],
  '👨': ['👨', '👨🏻', '👨🏼', '👨🏽', '👨🏾', '👨🏿'],
  '👩': ['👩', '👩🏻', '👩🏼', '👩🏽', '👩🏾', '👩🏿'],
  '👱‍♀️': ['👱‍♀️', '👱🏻‍♀️', '👱🏼‍♀️', '👱🏽‍♀️', '👱🏾‍♀️', '👱🏿‍♀️'],
  '👱': ['👱', '👱🏻', '👱🏼', '👱🏽', '👱🏾', '👱🏿'],
  '👴': ['👴', '👴🏻', '👴🏼', '👴🏽', '👴🏾', '👴🏿'],
  '👵': ['👵', '👵🏻', '👵🏼', '👵🏽', '👵🏾', '👵🏿'],
  '👲': ['👲', '👲🏻', '👲🏼', '👲🏽', '👲🏾', '👲🏿'],
  '👳‍♀️': ['👳‍♀️', '👳🏻‍♀️', '👳🏼‍♀️', '👳🏽‍♀️', '👳🏾‍♀️', '👳🏿‍♀️'],
  '👳': ['👳', '👳🏻', '👳🏼', '👳🏽', '👳🏾', '👳🏿'],
  '👮‍♀️': ['👮‍♀️', '👮🏻‍♀️', '👮🏼‍♀️', '👮🏽‍♀️', '👮🏾‍♀️', '👮🏿‍♀️'],
  '👮': ['👮', '👮🏻', '👮🏼', '👮🏽', '👮🏾', '👮🏿'],
  '👷‍♀️': ['👷‍♀️', '👷🏻‍♀️', '👷🏼‍♀️', '👷🏽‍♀️', '👷🏾‍♀️', '👷🏿‍♀️'],
  '👷': ['👷', '👷🏻', '👷🏼', '👷🏽', '👷🏾', '👷🏿'],
  '💂‍♀️': ['💂‍♀️', '💂🏻‍♀️', '💂🏼‍♀️', '💂🏽‍♀️', '💂🏾‍♀️', '💂🏿‍♀️'],
  '💂': ['💂', '💂🏻', '💂🏼', '💂🏽', '💂🏾', '💂🏿'],
  '🕵️‍♀️': ['🕵️‍♀️', '🕵🏻‍♀️', '🕵🏼‍♀️', '🕵🏽‍♀️', '🕵🏾‍♀️', '🕵🏿‍♀️'],
  '🕵️': ['🕵️', '🕵🏻', '🕵🏼', '🕵🏽', '🕵🏾', '🕵🏿'],
  '👩‍⚕️': ['👩‍⚕️', '👩🏻‍⚕️', '👩🏼‍⚕️', '👩🏽‍⚕️', '👩🏾‍⚕️', '👩🏿‍⚕️'],
  '👨‍⚕️': ['👨‍⚕️', '👨🏻‍⚕️', '👨🏼‍⚕️', '👨🏽‍⚕️', '👨🏾‍⚕️', '👨🏿‍⚕️'],
  '👩‍🌾': ['👩‍🌾', '👩🏻‍🌾', '👩🏼‍🌾', '👩🏽‍🌾', '👩🏾‍🌾', '👩🏿‍🌾'],
  '👨‍🌾': ['👨‍🌾', '👨🏻‍🌾', '👨🏼‍🌾', '👨🏽‍🌾', '👨🏾‍🌾', '👨🏿‍🌾'],
  '👩‍🍳': ['👩‍🍳', '👩🏻‍🍳', '👩🏼‍🍳', '👩🏽‍🍳', '👩🏾‍🍳', '👩🏿‍🍳'],
  '👨‍🍳': ['👨‍🍳', '👨🏻‍🍳', '👨🏼‍🍳', '👨🏽‍🍳', '👨🏾‍🍳', '👨🏿‍🍳'],
  '👩‍🎓': ['👩‍🎓', '👩🏻‍🎓', '👩🏼‍🎓', '👩🏽‍🎓', '👩🏾‍🎓', '👩🏿‍🎓'],
  '👨‍🎓': ['👨‍🎓', '👨🏻‍🎓', '👨🏼‍🎓', '👨🏽‍🎓', '👨🏾‍🎓', '👨🏿‍🎓'],
  '👩‍🎤': ['👩‍🎤', '👩🏻‍🎤', '👩🏼‍🎤', '👩🏽‍🎤', '👩🏾‍🎤', '👩🏿‍🎤'],
  '👨‍🎤': ['👨‍🎤', '👨🏻‍🎤', '👨🏼‍🎤', '👨🏽‍🎤', '👨🏾‍🎤', '👨🏿‍🎤'],
  '👩‍🏫': ['👩‍🏫', '👩🏻‍🏫', '👩🏼‍🏫', '👩🏽‍🏫', '👩🏾‍🏫', '👩🏿‍🏫'],
  '👨‍🏫': ['👨‍🏫', '👨🏻‍🏫', '👨🏼‍🏫', '👨🏽‍🏫', '👨🏾‍🏫', '👨🏿‍🏫'],
  '👩‍🏭': ['👩‍🏭', '👩🏻‍🏭', '👩🏼‍🏭', '👩🏽‍🏭', '👩🏾‍🏭', '👩🏿‍🏭'],
  '👨‍🏭': ['👨‍🏭', '👨🏻‍🏭', '👨🏼‍🏭', '👨🏽‍🏭', '👨🏾‍🏭', '👨🏿‍🏭'],
  '👩‍💻': ['👩‍💻', '👩🏻‍💻', '👩🏼‍💻', '👩🏽‍💻', '👩🏾‍💻', '👩🏿‍💻'],
  '👨‍💻': ['👨‍💻', '👨🏻‍💻', '👨🏼‍💻', '👨🏽‍💻', '👨🏾‍💻', '👨🏿‍💻'],
  '👩‍💼': ['👩‍💼', '👩🏻‍💼', '👩🏼‍💼', '👩🏽‍💼', '👩🏾‍💼', '👩🏿‍💼'],
  '👨‍💼': ['👨‍💼', '👨🏻‍💼', '👨🏼‍💼', '👨🏽‍💼', '👨🏾‍💼', '👨🏿‍💼'],
  '👩‍🔧': ['👩‍🔧', '👩🏻‍🔧', '👩🏼‍🔧', '👩🏽‍🔧', '👩🏾‍🔧', '👩🏿‍🔧'],
  '👨‍🔧': ['👨‍🔧', '👨🏻‍🔧', '👨🏼‍🔧', '👨🏽‍🔧', '👨🏾‍🔧', '👨🏿‍🔧'],
  '👩‍🔬': ['👩‍🔬', '👩🏻‍🔬', '👩🏼‍🔬', '👩🏽‍🔬', '👩🏾‍🔬', '👩🏿‍🔬'],
  '👨‍🔬': ['👨‍🔬', '👨🏻‍🔬', '👨🏼‍🔬', '👨🏽‍🔬', '👨🏾‍🔬', '👨🏿‍🔬'],
  '👩‍🎨': ['👩‍🎨', '👩🏻‍🎨', '👩🏼‍🎨', '👩🏽‍🎨', '👩🏾‍🎨', '👩🏿‍🎨'],
  '👨‍🎨': ['👨‍🎨', '👨🏻‍🎨', '👨🏼‍🎨', '👨🏽‍🎨', '👨🏾‍🎨', '👨🏿‍🎨'],
  '👩‍🚒': ['👩‍🚒', '👩🏻‍🚒', '👩🏼‍🚒', '👩🏽‍🚒', '👩🏾‍🚒', '👩🏿‍🚒'],
  '👨‍🚒': ['👨‍🚒', '👨🏻‍🚒', '👨🏼‍🚒', '👨🏽‍🚒', '👨🏾‍🚒', '👨🏿‍🚒'],
  '👩‍✈️': ['👩‍✈️', '👩🏻‍✈️', '👩🏼‍✈️', '👩🏽‍✈️', '👩🏾‍✈️', '👩🏿‍✈️'],
  '👨‍✈️': ['👨‍✈️', '👨🏻‍✈️', '👨🏼‍✈️', '👨🏽‍✈️', '👨🏾‍✈️', '👨🏿‍✈️'],
  '👩‍🚀': ['👩‍🚀', '👩🏻‍🚀', '👩🏼‍🚀', '👩🏽‍🚀', '👩🏾‍🚀', '👩🏿‍🚀'],
  '👨‍🚀': ['👨‍🚀', '👨🏻‍🚀', '👨🏼‍🚀', '👨🏽‍🚀', '👨🏾‍🚀', '👨🏿‍🚀'],
  '👩‍⚖️': ['👩‍⚖️', '👩🏻‍⚖️', '👩🏼‍⚖️', '👩🏽‍⚖️', '👩🏾‍⚖️', '👩🏿‍⚖️'],
  '👨‍⚖️': ['👨‍⚖️', '👨🏻‍⚖️', '👨🏼‍⚖️', '👨🏽‍⚖️', '👨🏾‍⚖️', '👨🏿‍⚖️'],
  '🤶': ['🤶', '🤶🏻', '🤶🏼', '🤶🏽', '🤶🏾', '🤶🏿'],
  '🎅': ['🎅', '🎅🏻', '🎅🏼', '🎅🏽', '🎅🏾', '🎅🏿'],
  '👸': ['👸', '👸🏻', '👸🏼', '👸🏽', '👸🏾', '👸🏿'],
  '🤴': ['🤴', '🤴🏻', '🤴🏼', '🤴🏽', '🤴🏾', '🤴🏿'],
  '👰': ['👰', '👰🏻', '👰🏼', '👰🏽', '👰🏾', '👰🏿'],
  '🤵 ': ['🤵', '🤵🏻', '🤵🏼', '🤵🏽', '🤵🏾', '🤵🏿']
};
/* jshint +W100 */
const randomItem = array => array[Math.floor(Math.random() * array.length)];
const randomAvatar = () => randomItem([].concat(...Object.values(AVATARS)));

const CONFIG_FILE = `${process.env.HOME}/.toggl.json`;

let configDirty = false;
const config = (() => {
  if (fs.existsSync(CONFIG_FILE)) {
    try {
      return JSON.parse(fs.readFileSync(CONFIG_FILE, 'utf8'));
    } catch (_) {}
  }

  // Defaults
  configDirty = true;
  return {
    avatar: randomAvatar(),
    hoursInDay: 8,
    daysInWeek: 5,
    style: 'hours'
  };
})();

const relativeThreshold = 15 * 60; // TODO: configurable?

const endOutput = () => {
  if (configDirty) {
    fs.writeFileSync(CONFIG_FILE, JSON.stringify(config, null, 2));
  }

  console.log('---');
  console.log('Refresh | refresh=true');
  process.exit();
};

const badApiToken = wrong => {
  console.log(`🚨 ${wrong ? 'provided api token is wrong' : 'token needed'} 🚨`);
  console.log('---');
  console.log('🖱 Click here to find your token| href=https://toggl.com/app/profile');
  console.log('It will be at the bottom of the page|size=12');
  console.log(`Once you've found your token, copy it (CMD+C)|size=12`);
  console.log(`🤞 I've copied it 🤞|bash=${process.argv[1]} param1=api_token refresh=true terminal=false `);
  endOutput();
};

const NOW = new Date();

const unix = date => Math.round(date.getTime() / 1000);
const outputUnix = (unixTime, verbose) => {
  const negative = (() => {if (unixTime < 0) {
    // TODO: turn red if it's negative
    unixTime *= -1;
    return true;
  }})();

  const fmt = x => x.toLocaleString(undefined, {minimumIntegerDigits: 2});
  const hours = Math.floor(unixTime / 60 / 60);
  const minutes = Math.floor(unixTime / 60) - (hours * 60);

  if (verbose) {
    return `${negative ? '-' : ''}${hours} hours ${minutes} minutes`;
  } else {
    return `${negative ? '-' : ''}${hours}:${fmt(minutes)}`;
  }
};

// TODO: alter this so you can pass the considered start day (ie Sunday or Monday)
//       Might as well be the offset integer, 0 or 1, but in theory could be 0-6
const startOfWeek = () => {
  const thisWeek = new Date(NOW.getFullYear(), NOW.getMonth(), NOW.getDate() - NOW.getDay());
  return unix(thisWeek);
};

const avatar = () => config.avatar === 'avatar' ?  randomAvatar() : config.avatar;

const outputHeader = (timeDay, timeWeek) => {
  let daySection; // Output string
  let dayAmount; // single amount, if possible
  if (typeof timeDay === 'object') {
    const max = Math.max(...timeDay);
    const min = Math.min(...timeDay);
    if ((max - min) > relativeThreshold) {
      daySection = `${outputUnix(min)} — ${outputUnix(max)}`;
    } else {
      dayAmount = min + Math.round((max - min) / 2);
      daySection = `${outputUnix(dayAmount)}`;
    }
  } else {
    dayAmount = timeDay;
    daySection = `${outputUnix(dayAmount)}`;
  }

  if (dayAmount && Math.abs(timeWeek - dayAmount) > relativeThreshold) {
    console.log(`${avatar()} ${daySection} (${outputUnix(timeWeek)})`);
  } else {
    console.log(`${avatar()} ${daySection}`);
  }
};

const displayTimes = me => {
  // Calculate times
  const unixToday = unix(new Date(NOW.getFullYear(), NOW.getMonth(), NOW.getDate()));
  const completeDay = config.hoursInDay * 60 * 60;
  const completeWeek = completeDay * config.daysInWeek;

  let full = 0,
      today = 0;
  const days = [];

  let currentlyWorking,
      currentWid,
      currentPid;

  const timeByWidByPid = {};

  (me.data.time_entries || []).forEach(({start, duration:entryDuration, wid, pid}) => {
    // TODO: deal with partial entries that cross over midnight
    //       (both daily and weekly)
    // TODO: respect configured start of week in me.beginning_of_week
    //
    start = new Date(start);

    let duration;
    if (entryDuration > 0) {
      duration = entryDuration;
    } else {
      duration = unix(NOW) - unix(start);
      currentlyWorking = true;
      currentWid = wid;
      currentPid = pid;
    }

    if (!timeByWidByPid[wid]) {
      timeByWidByPid[wid] = {};
    }
    if (!timeByWidByPid[wid][pid]) {
      timeByWidByPid[wid][pid] = 0;
    }
    timeByWidByPid[wid][pid] += duration;

    if (unix(start) > startOfWeek()) {
      full += duration;
      let day = start.getDay();
      const end = new Date(start.getTime() + duration * 1000);
      if (day === end.getDay()) {
        days[day] = (days[day] || 0) + duration;
      } else {
        // TODO: We want to split a duration over midnights and distribute them to the correct days
        days[day] = (days[day] || 0) + duration;
      }
    }

    if (unix(start) > unixToday) {
      today += duration;
    }
  });

  // Output times
  if (!currentlyWorking) {
    process.exit(0);
  }

  switch(config.style) {
    case 'hours': {
      outputHeader(today, full);
      break;
    }
    case 'left': {
      outputHeader(completeDay - today, completeWeek - full);
      break;
    }
    case 'percentage': {
      const completeDay = config.hoursInDay * 60 * 60;
      const completeWeek = completeDay * config.daysInWeek;
      const dayPercent = Math.round((today / completeDay) * 100);
      const weekPercent = Math.round((full / completeWeek) * 100);
      console.log(`${avatar()} ${dayPercent}% (${weekPercent}%)`);
      break;
    }
    case 'relative': {
      const startOfWeekday = 1; // TODO: support using configured day from /me
      const todayWeekday = NOW.getDay();
      const daysLeft = config.daysInWeek - (todayWeekday - startOfWeekday);

      const timeInWeek = config.daysInWeek * config.hoursInDay * 60 * 60;

      const allButTodaysTime = full - today;
      const allButTodaysTimeLeft = timeInWeek - allButTodaysTime;
      const timePerDayLeft = Math.round(allButTodaysTimeLeft / daysLeft);
      const amortisedTimeLeft = timePerDayLeft - today;

      const onTrackTime = (daysLeft - 1) * config.hoursInDay * 60 * 60;
      const timeOffTrack = allButTodaysTimeLeft - onTrackTime - today;
      const fullDay = Math.min(completeDay - today, timeInWeek - full);

      outputHeader([amortisedTimeLeft, timeOffTrack, fullDay], timeInWeek - full);
      break;
    }
  }

  // TODO: add end day time, either as its own option, or as a custom estimate
  //       in the menu.
  //       I think having it as an option might be a bit more chill, you don't
  //       see it counting down, just an idea of when knocking off time is

  // Have to filter first before showing length because Monday is idx 1 / length 2
  if (days.filter(d => !!d).length > 1) {
    console.log('---');
    const dayNames = ['S', 'M', 'T', 'W', 'T', 'F', 'S'];
    days.forEach((val, day) => {
      console.log(`${dayNames[day]}:\t${outputUnix(val, true)}`);
    });
    console.log(`Σ:\t${outputUnix(full, true)}`);
    console.log(`T-:\t${outputUnix(completeWeek - full, true)}`);
  }

  // TODO: figure out how we want to pull in wids and pid labels, and if it's worth the extra api calls
  // console.log('---');
  // console.log(`${currentWid} :: ${currentPid}`);
  // console.log(outputUnix(timeByWidByPid[currentWid][currentPid], true));

  // TODO: display project summary for the week
  // TODO: allow muting of a project as it relates to time
  //       (if you're on a muted project just show emoji but not time)
  //       (it would still show in this project summary)
};

const avatarChoice = () => {
  console.log(`Change ${config.avatar}`);
  console.log(`--((surprise me))|bash=${process.argv[1]} param1=avatar param2=avatar refresh=true terminal=false size=10`);
  Object.keys(AVATARS).forEach(k => {
    console.log(`--${k}|size=32`);
    AVATARS[k].forEach(v => {
      console.log(`----${v}|bash=${process.argv[1]} param1=avatar param2=${v} refresh=true terminal=false size=32`);
    });
  });
};

const styleChoice = () => {
  const current = style => style === config.style ? '✓ ' : '';
  const link = style => `|bash=${process.argv[1]} param1=style param2=${style} refresh=true terminal=false`;
  console.log('Change reporting style');
  console.log(`--${current('hours')}Hours complete${link('hours')}`);
  console.log(`--${current('left')}Hours left${link('left')}`);
  console.log(`--${current('percentage')}Percentage complete${link('percentage')}`);
  console.log(`--${current('relative')}Relative weekly goals${link('relative')}`);
};

const input = () => {
  switch (process.argv[2]) {
    case 'avatar': {
      config.avatar = process.argv[3];
      configDirty = true;
      break;
    }
    case 'api_token': {
      config.apiToken = require('child_process').execSync('pbpaste').toString();
      configDirty = true;
      break;
    }
    case 'style': {
      config.style = process.argv[3];
      configDirty = true;
      break;
    }
  }
};

const output = () => {
  require('https').get({
    hostname: 'toggl.com',
    // NB: since is "edited since", and so isn't really reliable
    path: `/api/v8/me?with_related_data=true&since=${startOfWeek()}`,
    auth: `${config.apiToken}:api_token`
  }, res => {
    if (res.statusCode === 403) {
      badApiToken(true);
      endOutput();
    }

    let body = '';
    res.on('data', data => body += data);
    res.on('end', () => {
      try {
        displayTimes(JSON.parse(body));
        console.log('---');
        avatarChoice();
        styleChoice();
        endOutput();
      } catch (error) {
        console.log(':-(');
        console.log('---');
        console.log(error);
        console.log('---');
        console.log(body);
        endOutput();
      }
    });
    res.on('error', err => {
      console.log(':-(');
      console.log('---');
      console.log(err);
      endOutput();
    });
  });
};

input();
if (!config.apiToken) {
  badApiToken();
}
output();
