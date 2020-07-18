#!/usr/bin/env /usr/local/bin/node

// <bitbar.title>Your Team: Soccer Matches</bitbar.title>
// <bitbar.version>v1.0</bitbar.version>
// <bitbar.author>Taylor Sturtz</bitbar.author>
// <bitbar.author.github>tsturtz</bitbar.author.github>
// <bitbar.desc>Show completed/upcoming and LIVE matches as well as competition standings for your favorite soccer team.</bitbar.desc>
// <bitbar.image>https://raw.githubusercontent.com/tsturtz/your-team-soccer-matches/master/your-team-soccer-matches.jpg</bitbar.image>
// <bitbar.dependencies>node,npm/bitbar,npm/date-fns,npm/node-fetch</bitbar.dependencies>
// <bitbar.abouturl>https://github.com/tsturtz/your-team-soccer-matches</bitbar.abouturl>

// -----------------------------------------------------------------------------
// 🙋‍♂️ Start here!
// -----------------------------------------------------------------------------
// 1. Ensure you have the proper node (tested on v12.16.3) and npm dependencies installed.
//    - npm install --global bitbar node-fetch date-fns
// 2. Get a free API key from https://www.football-data.org/client/register.
// 3. Configure the USER_OPTIONS object.
//    - FOOTBALL_DATA_API_KEY: Provide your API key you just got here.
//    - TEAM_ID: Provide an accurate TEAM_ID or it will default to my team, Tottenham 😎.
//      - Use the a dictionary of *some* teams and their IDs below to find your team's ID.
//    - NUMBER_OF_FINISHED_MATCHES: The number of completed matches to retrieve and display.
//      - DEFAULT: 5; MAX: 7;
//    - NUMBER_OF_SCHEDULED_MATCHES: The number of upcoming matches to retrieve and display.
//      - DEFAULT: 3; NO MAX;

// -----------------------------------------------------------------------------
// 👀 Below are some incomplete but butter-than-nothing team id lookup dictionaries.
// Search the file to get your team's football-data ID, but don't touch this code.
// -----------------------------------------------------------------------------
const NATIONAL_TEAM_IDS = { Argentina: 762, Australia: 779, Belgium: 805, Brazil: 764, Colombia: 818, Costa_Rica: 793, Croatia: 799, Denmark: 782, Egypt: 825, England: 770, France: 773, Germany: 759, Iceland: 1066, Iran: 840, Japan: 766, Korea_Republic: 772, Mexico: 769, Morocco: 815, Nigeria: 776, Panama: 1836, Peru: 832, Poland: 794, Portugal: 765, Russia: 808, Saudi_Arabia: 801, Senegal: 804, Serbia: 780, Spain: 760, Sweden: 792, Switzerland: 788, Tunisia: 802, Uruguay: 758, };
const GERMAN_LEAGUE_TEAM_IDS = { Augsburg: 16, Bayer_04_Leverkusen: 3, Bayern_München: 5, Borussia_Mönchengladbach: 18, Bremen: 12, Dortmund: 4, FC_Köln: 1, Frankfurt: 19, Freiburg: 17, Hannover: 8, Hertha_BSC: 9, Hoffenheim: 2, HSV: 7, Kaiserslautern: 13, Mainz: 15, Nürnberg: 14, RB_Leipzig: 721, RB_Salzburg: 1877, Schalke_04: 6, Stuttgart: 10, Wolfsburg: 11, };
const ENGLISH_LEAGUE_TEAM_IDS = { Arsenal: 57, Aston_Villa: 58, Barnsley: 357, Birmingham: 332, Blackburn_Rovers: 59, Bolton_Wanderers: 60, Bournemouth: 1044, Brentford: 402, Brighton_And_Hove_Albion: 397, Bristol_City: 387, Burnley: 328, Burton_Albion: 1072, Charlton: 348, Chelsea: 61, Crystal_Palace: 354, Derby: 342, England: 770, Everton: 62, Fulham: 63, Huddersfield: 394, Hull_City: 322, Ipswich_Town: 349, Leeds: 341, Leicester_City: 338, Liverpool: 64, Luton_Town: 389, Manchester_City: 65, Manchester_United: 66, Middlesbrough: 343, Millwall: 384, Newcastle: 67, Norwich: 68, Nottingham_Forest: 351, Preston_North_End: 1081, QPR: 69, Reading: 355, Rotherham_Utd: 385, Sheffield_United: 356, Sheffield_Wednesday: 345, Southampton: 340, Stoke: 70, Sunderland: 71, Swansea: 72, Tottenham_Hotspur: 73, Watford: 346, West_Brom: 74, West_Ham: 563, Wigan: 75, Wolverhampton: 76, };
const SPANISH_LEAGUE_TEAM_IDS = { Atlético_de_Madrid: 78, Barcelona: 81, Real_Madrid: 86, Valencia: 95, };
const ITALY_LEAGUE_TEAM_IDS = { Atalanta: 102, Inter: 108, Juventus: 109, Napoli: 113, };
const FRANCE_LEAGUE_TEAM_IDS = { Lille: 521, Lyon: 523, PSG: 524, };
const NETHERLANDS_LEAGUE_TEAM_IDS = { Ajax: 678, PSV: 674, };
const MISC_LEAGUE_TEAM_IDS = { AIK_Fotboll: 5277, APOEL: 752, Astana_FK: 1884, Başakşehir: 1897, Basel: 729, BATE: 748, Celtic: 732, Club_Brugge: 851, Crvena_Zvedza: 7283, Dinamo_Zagreb: 755, Dundalk: 1873, Dynamo_Kyiv: 842, F91_Dudelange: 1875, Galatasaray: 610, HJK: 5123, København: 1876, KRC_Genk: 1858, LASK: 2016, Linfield: 1896, Ludogorets: 1901, Maribor: 734, Nõmme_Kalju: 5106, Olympiakos: 654, PAOK_FC: 6146, Porto: 503, Qarabağ_Ağdam: 611, Rosenborg: 889, Santa_Coloma: 1879, Sarajevo: 4275, Shaktar: 1887, Sheriff: 1880, SL_Benfica: 1903, Slavia_Praha: 930, The_New_Saints: 1904, Viktoria_Plzeň: 1881, Young_Boys: 1871, Zenit: 731, };

// -----------------------------------------------------------------------------
// ✍️ CONFIGURE API KEY AND OPTIONS HERE!
// -----------------------------------------------------------------------------
const USER_OPTIONS = {
  FOOTBALL_DATA_API_KEY: '',
  TEAM_ID: ENGLISH_LEAGUE_TEAM_IDS.Tottenham_Hotspur,
  NUMBER_OF_FINISHED_MATCHES: 5,
  NUMBER_OF_SCHEDULED_MATCHES: 3,
};

// -----------------------------------------------------------------------------
// 🚨 You shouldn't need to change anything below this line.
// -----------------------------------------------------------------------------

// modules are defined as an array
// [ module function, map of requires ]
//
// map of requires is short require name -> numeric require
//
// anything defined in a previous bundle is accessed via the
// orig method which is the require for previous bundles
parcelRequire = (function (modules, cache, entry, globalName) {
  // Save the require from previous bundle to this closure if any
  var previousRequire = typeof parcelRequire === 'function' && parcelRequire;
  var nodeRequire = typeof require === 'function' && require;

  function newRequire(name, jumped) {
    if (!cache[name]) {
      if (!modules[name]) {
        // if we cannot find the module within our internal map or
        // cache jump to the current global require ie. the last bundle
        // that was added to the page.
        var currentRequire = typeof parcelRequire === 'function' && parcelRequire;
        if (!jumped && currentRequire) {
          return currentRequire(name, true);
        }

        // If there are other bundles on this page the require from the
        // previous one is saved to 'previousRequire'. Repeat this as
        // many times as there are bundles until the module is found or
        // we exhaust the require chain.
        if (previousRequire) {
          return previousRequire(name, true);
        }

        // Try the node require function if it exists.
        if (nodeRequire && typeof name === 'string') {
          return nodeRequire(name);
        }

        var err = new Error('Cannot find module \'' + name + '\'');
        err.code = 'MODULE_NOT_FOUND';
        throw err;
      }

      localRequire.resolve = resolve;
      localRequire.cache = {};

      var module = cache[name] = new newRequire.Module(name);

      modules[name][0].call(module.exports, localRequire, module, module.exports, this);
    }

    return cache[name].exports;

    function localRequire(x){
      return newRequire(localRequire.resolve(x));
    }

    function resolve(x){
      return modules[name][1][x] || x;
    }
  }

  function Module(moduleName) {
    this.id = moduleName;
    this.bundle = newRequire;
    this.exports = {};
  }

  newRequire.isParcelRequire = true;
  newRequire.Module = Module;
  newRequire.modules = modules;
  newRequire.cache = cache;
  newRequire.parent = previousRequire;
  newRequire.register = function (id, exports) {
    modules[id] = [function (require, module) {
      module.exports = exports;
    }, {}];
  };

  var error;
  for (var i = 0; i < entry.length; i++) {
    try {
      newRequire(entry[i]);
    } catch (e) {
      // Save first error but execute all entries
      if (!error) {
        error = e;
      }
    }
  }

  if (entry.length) {
    // Expose entry point to Node, AMD or browser globals
    // Based on https://github.com/ForbesLindesay/umd/blob/master/template.js
    var mainExports = newRequire(entry[entry.length - 1]);

    // CommonJS
    if (typeof exports === "object" && typeof module !== "undefined") {
      module.exports = mainExports;

    // RequireJS
    } else if (typeof define === "function" && define.amd) {
     define(function () {
       return mainExports;
     });

    // <script>
    } else if (globalName) {
      this[globalName] = mainExports;
    }
  }

  // Override the current require with this new one
  parcelRequire = newRequire;

  if (error) {
    // throw error from earlier, _after updating parcelRequire_
    throw error;
  }

  return newRequire;
})({"Focm":[function(require,module,exports) {
function ownKeys(object, enumerableOnly) { var keys = Object.keys(object); if (Object.getOwnPropertySymbols) { var symbols = Object.getOwnPropertySymbols(object); if (enumerableOnly) symbols = symbols.filter(function (sym) { return Object.getOwnPropertyDescriptor(object, sym).enumerable; }); keys.push.apply(keys, symbols); } return keys; }

function _objectSpread(target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i] != null ? arguments[i] : {}; if (i % 2) { ownKeys(Object(source), true).forEach(function (key) { _defineProperty(target, key, source[key]); }); } else if (Object.getOwnPropertyDescriptors) { Object.defineProperties(target, Object.getOwnPropertyDescriptors(source)); } else { ownKeys(Object(source)).forEach(function (key) { Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key)); }); } } return target; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

const bitbar = require('bitbar');

const fetch = require('node-fetch').default;

const {
  format,
  parseISO
} = require('date-fns');

const OPTIONS = {
  FOOTBALL_DATA_API_KEY: USER_OPTIONS && USER_OPTIONS.FOOTBALL_DATA_API_KEY,
  TEAM_ID: USER_OPTIONS && USER_OPTIONS.TEAM_ID || 73,
  NUMBER_OF_FINISHED_MATCHES: USER_OPTIONS && USER_OPTIONS.NUMBER_OF_FINISHED_MATCHES || 5,
  NUMBER_OF_SCHEDULED_MATCHES: USER_OPTIONS && USER_OPTIONS.NUMBER_OF_SCHEDULED_MATCHES || 3
};

if (!OPTIONS.FOOTBALL_DATA_API_KEY) {
  console.log('MISSING API KEY');
}

if (OPTIONS.NUMBER_OF_FINISHED_MATCHES > 7) {
  OPTIONS.NUMBER_OF_FINISHED_MATCHES = 7;
}

(async () => {
  const apiUrl = 'https://api.football-data.org/v2';
  const apiData = {
    method: 'get',
    headers: {
      'Content-Type': 'application/json',
      'X-Auth-Token': OPTIONS.FOOTBALL_DATA_API_KEY
    }
  }; // Get team info

  const myTeamResponse = await fetch(`${apiUrl}/teams/${OPTIONS.TEAM_ID}`, apiData);
  const myTeam = await myTeamResponse.json(); // Get active competitions standings
  // NOTE: The free API only supports some competitions: https://www.football-data.org/coverage

  const supportedCompetitions = new Set(['BSA', 'PL', 'ELC', 'CL', 'EC', 'FL1', 'BL1', 'SA', 'DED', 'PPL', 'PD', 'WC']);
  const activeCompetitionsPromises = myTeam.activeCompetitions.filter(comp => supportedCompetitions.has(comp.code)).map(comp => fetch(`${apiUrl}/competitions/${comp.code}/standings?standingType=TOTAL`, apiData));
  const activeCompetitionsPromisesResponses = await Promise.all(activeCompetitionsPromises);
  const activeCompetitionsStandings = await Promise.all(activeCompetitionsPromisesResponses.map(resp => resp.json()));
  let activeCompetitionsStandingsRender = [];
  activeCompetitionsStandings.forEach(comp => {
    activeCompetitionsStandingsRender = [...activeCompetitionsStandingsRender, {
      text: comp.competition.name,
      href: `https://www.google.com/search?q=${comp.competition.name.split(' ').join('+')}`,
      size: 14,
      submenu: comp.standings.map(standing => ({
        text: standing.stage === 'GROUP_STAGE' ? standing.group : standing.stage,
        size: 14,
        submenu: [{
          text: 'Pos. Team - Points'
        }, bitbar.separator, ...standing.table.map((table, idx) => {
          return _objectSpread({
            text: `${table.position}. ${table.team.name} - ${table.points}`,
            href: `https://www.google.com/search?q=${table.team.name.split(' ').join('+')}`
          }, table.team.id === OPTIONS.TEAM_ID && {
            font: 'Helvetica-Bold'
          });
        })]
      }))
    }];
  }); // Get matches

  const matchesResponse = await fetch(`${apiUrl}/teams/${OPTIONS.TEAM_ID}/matches`, apiData);
  const matchesResponseJson = await matchesResponse.json();
  const finishedMatches = matchesResponseJson.matches.filter(match => match.status === 'FINISHED');
  const scheduledMatches = matchesResponseJson.matches.filter(match => match.status === 'SCHEDULED');
  const liveMatches = matchesResponseJson.matches.filter(match => match.status === 'LIVE' || match.status === 'IN_PLAY' || match.status === 'PAUSED'); // Finished/completed matches

  let finishedMatchesRender = [];

  if (finishedMatches) {
    let idx = 0; // Use for..of here instead of forEach because it plays nice with async/await

    for (const match of finishedMatches) {
      if (idx >= finishedMatches.length - OPTIONS.NUMBER_OF_FINISHED_MATCHES) {
        // Determine winner
        const isDraw = match.score.winner === 'DRAW';
        let winningTeamName = 'Draw';

        if (match.score.winner === 'HOME_TEAM') {
          winningTeamName = match.homeTeam.name;
        } else if (match.score.winner === 'AWAY_TEAM') {
          winningTeamName = match.awayTeam.name;
        }

        const myTeamWon = winningTeamName === myTeam.name;
        const myTeamIsHome = match.homeTeam.name === myTeam.name; // Get TLA

        const opponentTeamId = myTeamIsHome ? match.awayTeam.id : match.homeTeam.id;
        const opponentTeamResponse = await fetch(`${apiUrl}/teams/${opponentTeamId}`, apiData);
        const opponentTeam = await opponentTeamResponse.json(); // Check if there was extra time

        const showExtraTime = match.score.extraTime.homeTeam || match.score.extraTime.awayTeam; // Check if there were penalties

        const showPenalties = match.score.penalties.homeTeam || match.score.penalties.awayTeam; // Render regular scores

        const regularScoreRender = `${myTeamIsHome ? myTeam.tla : opponentTeam.tla} ${match.score.fullTime.homeTeam} - ${match.score.fullTime.awayTeam} ${myTeamIsHome ? opponentTeam.tla : myTeam.tla}`; // Render extra time scores

        const extraTimeRender = `(ET: ${match.score.extraTime.homeTeam} - ${match.score.extraTime.awayTeam})`; // Render extra time scores

        const penaltiesRender = `(Pen: ${match.score.penalties.homeTeam} - ${match.score.penalties.awayTeam})`;
        finishedMatchesRender = [...finishedMatchesRender, {
          text: `${match.homeTeam.name} vs. ${match.awayTeam.name}`,
          size: 14,
          href: `https://www.google.com/search?q=${match.homeTeam.name.split(' ').join('+')}+vs.+${match.awayTeam.name.split(' ').join('+')}`
        }, {
          text: `${myTeamWon ? '🟢' : isDraw ? '⚪️' : '🔴'} ${regularScoreRender} ${showExtraTime ? extraTimeRender : ''} ${showPenalties ? penaltiesRender : ''}`,
          size: 14
        }];
      }

      idx++;
    }
  } // Scheduled/upcoming matches


  let scheduledMatchesRender = [];

  if (scheduledMatches) {
    scheduledMatches.forEach((match, idx) => {
      if (idx < OPTIONS.NUMBER_OF_SCHEDULED_MATCHES) {
        scheduledMatchesRender = [...scheduledMatchesRender, {
          text: `${match.homeTeam.name} vs. ${match.awayTeam.name}`,
          size: 14,
          href: `https://www.google.com/search?q=${match.homeTeam.name.split(' ').join('+')}+vs.+${match.awayTeam.name.split(' ').join('+')}`
        }, {
          text: `${match.competition.name} - ${match.group || match.stage}${match.matchday ? ` - Match day: ${match.matchday}` : ''}`,
          size: 14
        }, {
          text: `${format(parseISO(match.utcDate), 'MM/dd/yyyy - hh:mm a')}`,
          size: 14
        }];
      }
    });
  } // Live matches


  let liveMatchesRender = [];

  if (liveMatches) {
    liveMatches.forEach(async match => {
      liveMatchesRender = [...liveMatchesRender, {
        text: `${match.homeTeam.name} vs. ${match.awayTeam.name}`,
        size: 14,
        href: `https://www.google.com/search?q=${match.homeTeam.name.split(' ').join('+')}+vs.+${match.awayTeam.name.split(' ').join('+')}`
      }, {
        text: `${match.competition.name} - ${match.group || match.stage}${match.matchday ? ` - Match day: ${match.matchday}` : ''}`,
        size: 14
      }, {
        text: 'Click here to check the score',
        size: 14,
        href: `https://www.google.com/search?q=${match.homeTeam.name.split(' ').join('+')}+vs.+${match.awayTeam.name.split(' ').join('+')}`
      }];
    });
  } // Prepare render sections


  const activeCompetitionsStandingsSection = activeCompetitionsStandingsRender.length ? [bitbar.separator, {
    text: 'Standings',
    size: 22
  }, ...activeCompetitionsStandingsRender] : [bitbar.separator, {
    text: 'Standings',
    size: 22
  }, {
    text: 'No active competitions.',
    size: 14
  }];
  const finishedMatchesSection = finishedMatchesRender.length ? [bitbar.separator, {
    text: 'Completed Matches',
    size: 22
  }, ...finishedMatchesRender] : [bitbar.separator, {
    text: 'Completed Matches',
    size: 22
  }, {
    text: 'No recently completed matches.',
    size: 14
  }];
  const scheduledMatchesSection = scheduledMatchesRender.length ? [bitbar.separator, {
    text: 'Upcoming Matches',
    size: 22
  }, ...scheduledMatchesRender] : [bitbar.separator, {
    text: 'Upcoming Matches',
    size: 22
  }, {
    text: 'No upcoming matches.',
    size: 14
  }];
  const liveMatchesSection = liveMatchesRender.length ? [bitbar.separator, {
    text: 'Live Matches',
    size: 22
  }, ...liveMatchesRender] : [bitbar.separator, {
    text: 'Live Matches',
    size: 22
  }, {
    text: 'No live matches right now.',
    size: 14
  }]; // Render the bitbar dropdown

  bitbar([{
    text: `⚽︎ ${myTeam.tla}`,
    dropdown: false
  }, bitbar.separator, {
    text: myTeam.name,
    size: 30,
    href: myTeam.website
  }, ...activeCompetitionsStandingsSection, ...finishedMatchesSection, ...scheduledMatchesSection, ...liveMatchesSection]);
})();
},{}]},{},["Focm"], null)