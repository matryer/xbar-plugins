#!/usr/bin/env php
<?php

# <bitbar.title>Cricket Bar</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Adi</bitbar.author>
# <bitbar.author.github>gomedia-adi</bitbar.author.github>
# <bitbar.desc>Displays live cricket score of a selected match, using data from ESPNcricinfo or Cricbuzz.</bitbar.desc>
# <bitbar.image>http://www.greatoceanmedia.com.au/images/62.png</bitbar.image>
# <bitbar.dependencies>php</bitbar.dependencies>
# <bitbar.abouturl>http://www.greatoceanmedia.com.au/bitbar</bitbar.abouturl>

// Inspired by, and respect to Anup Sam Abraham's Live Cricket Scores v1.1 (https:getbitbar.com/plugins/Sports/live_cricket.2m.py)
// BitBar plugin help: https:github.com/matryer/bitbar
//
// Plugin updated 25/11/17
//
// Version history
// 1.1	- option to set timezone
//		- selected match ID reset on feed change
//		- timeout set on Curl connect
// 1.0	- initial release

//TODO 
// user specific config/log files
// option to hide scores in menubar
// weather link?
// score format option - Standard, Australia
// check out [break] in ESPNcricinfo - more informative than live_state ("Stumps" vs "Stumps - Day 3")?

//??? KNOWN ISSUES
//
// General
// - long menubar displays (e.g. matches with two innings & scores per team) may "disappear" due to lack of room with other menubar items and application menus
//		- workaround: switch to application with fewer menus, e.g. Finder
//		- future enhancement: option to hide scores in menubar
//
// ESPNcricinfo
// - home team not always first in summary
//		- workaround: none
//		- fix: home team info in detailed feed
// - summary score doesn't keep pace with detailed feed score (not sure of bug or deliberate on ESPNcricinfo's part)
//		- workaround: none
//		- fix: only use detailed feed (ties in with previous fix)
//
// Cricbuzz
// - last wicket dismissal information not available
// 		- workaround/fix: none
// - duplicate entries in current match list
//		- workaround: none
//		- fix: may be difficult - a completed match can have two match IDs
// - full team names rarely supplied
//		- workaround/fix: hardcode translations
// - feed only provides limited match types: TEST, ODI, T20
//		- workaround/fix: none


define('UA', 'Macintosh; Intel Mac OS X 10_12_4) AppleWebKit/603.1.30 (KHTML, like Gecko) Version/10.1 Safari/603.1.30');
define('CONFIG_FILE', "/var/tmp/bitbar.cricket_bar.config.php");
define('LOG_FILE', "/var/tmp/bitbar.cricket_bar.log");
define('DEBUG_FILE', "/var/tmp/bitbar.cricket_bar.debug.log");

global $abbrev_map;

function update_config($selected_match_id, $feed, $timezone) {
// rewrite config file with given values
	file_put_contents(CONFIG_FILE, "<?php\n");
	file_put_contents(CONFIG_FILE, "// CRICKET_BAR CONFIGURATION\n", FILE_APPEND);
	file_put_contents(CONFIG_FILE, "\$feed = '$feed';\n", FILE_APPEND);
	file_put_contents(CONFIG_FILE, "\$selected_match_id = '$selected_match_id';\n", FILE_APPEND);
	file_put_contents(CONFIG_FILE, "\$timezone = '$timezone';\n", FILE_APPEND);
	file_put_contents(CONFIG_FILE, "?>\n", FILE_APPEND);
}

function tz_error_handler($errno, $errstr) {
// fix for PHP timezone config problem
	$mac_timezone_abbr = exec('date +%Z'); // get local Mac timezone abbreviation, e.g. AEST
	file_put_contents(LOG_FILE, '**('.__FUNCTION__.') MAC TIMEZONE ABBR: '.$mac_timezone_abbr."\n", FILE_APPEND);
	$mac_timezone_name = timezone_name_from_abbr(exec('date +%Z')); // convert timezone abbreviation to name, e.g. Australia/Melbourne
	file_put_contents(LOG_FILE, '**('.__FUNCTION__.') SET PHP TO MAC TIMEZONE NAME: '.$mac_timezone_name."\n", FILE_APPEND);
	date_default_timezone_set($mac_timezone_name); // set PHP timezone
}

function curl_obj($url) {
// curly whirly
	$ch = curl_init();
	curl_setopt($ch, CURLOPT_URL, $url);
	curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
	curl_setopt($ch, CURLOPT_USERAGENT, UA);
	curl_setopt($ch, CURLOPT_FAILONERROR, TRUE);
	curl_setopt($ch, CURLOPT_CONNECTTIMEOUT, 10); // set connection timeout (workaround for long connection delays - seen during 1st Ashes Test on Cricbuzz - data is still received though)
	$output = curl_exec($ch);
	file_put_contents(LOG_FILE, "**(".__FUNCTION__.") CURL: ".$url."\n", FILE_APPEND);
	if (curl_errno($ch)) // may not pick up website problems though
		file_put_contents(LOG_FILE, "**CURL ERROR: ".curl_error($ch)."\n", FILE_APPEND);
	curl_close($ch);
	// use URL extension to determine decode action
	$path_parts = pathinfo($url);
	file_put_contents(LOG_FILE, "**(".__FUNCTION__.") DATA FORMAT: ", FILE_APPEND);
	if ($path_parts['extension'] == 'json') {
		file_put_contents(LOG_FILE, "JSON\n", FILE_APPEND);
		return json_decode($output);
	}
	else if ($path_parts['extension'] == 'xml') {
		file_put_contents(LOG_FILE, "XML\n", FILE_APPEND);
		return @simplexml_load_string($output); // suppress warnings/errors, otherwise everything goes pear shaped - can occur if XML feed contains badly formatted data (e.g. & instead of &amp;)
	}
	else {
		file_put_contents(LOG_FILE, "UNKNOWN\n", FILE_APPEND);
		return '';
	}
}

function get_batsman($detailed_feed, $player_id) {
// return batsman's name from given player ID (ESPNcricinfo)
	$name = '';
	foreach($detailed_feed->team as $team) {
		foreach ($team->player as $player) {
			if ($player->player_id == $player_id) {
				$name = $player->card_long;
				break;
			}
		}
		if ($name) break;
	}
	return ($name ? $name : '?');
}

function translate_name($abbrev) {
// replace given abbreviation with translation if exists (ESPNcricinfo & Cricbuzz)
	global $abbrev_map;

	$this_abbrev = strtoupper($abbrev);
	if (array_key_exists($this_abbrev, $abbrev_map))
		return $abbrev_map[$this_abbrev];
	else
		return $abbrev;
}

function translate_names($string) {
// translate all abbreviations in a string (Cricbuzz)
	$words = explode(' ', $string);
	foreach ($words as $index => $word)
		$words[$index] = translate_name($word);
	return implode(' ', $words);
}

function projected_score($noofovers, $overs, $score) {
// calculate projected score based on no. of overs in match, current over & current score (ESPNcricinfo & Cricbuzz)
	if (strpos($overs,'.') === FALSE)
		$balls = (int)$overs * 6;
	else {
		$parts = explode('.',$overs);
		$balls = $parts[0] * 6 + $parts[1];
	}
	$proj_score = round($score / ($balls / 6) * $noofovers);
	return " Proj Scr $proj_score";
}

//-------------------------------------------------------------------------------------------------

//??? CONFIG FILE

// initialise config file
if (!file_exists(CONFIG_FILE))
	touch(CONFIG_FILE);

// read config file
include(CONFIG_FILE);

// provide defaults
if (!isset($feed))
	$feed = 'espncricinfo';
if (!isset($selected_match_id))
	$selected_match_id = '';
if (!isset($timezone))
	$timezone = '';

// update config (value supplied as argument, update config file, exit script)
if (isset($argv[1])) { // match, feed or timezone
	if (isset($argv[2])) { // feed or timezone
		if (isset($argv[3])) { // timezone
			update_config($selected_match_id, $feed, $argv[3]);
		}
		else { // feed
			update_config('unknown', $argv[2], $timezone); // reset selected match when changing feed
		}
	}
	else { // match
		update_config($argv[1], $feed, $timezone);
	}
	exit;
}

//??? INITIALISE

// initialise logfile
file_put_contents(LOG_FILE, '**CRICKET_BAR BITBAR PLUGIN'."\n");
file_put_contents(LOG_FILE, '**CONFIG FEED: '.$feed."\n", FILE_APPEND);
file_put_contents(LOG_FILE, '**CONFIG SELECTED_MATCH_ID: '.$selected_match_id."\n", FILE_APPEND);
file_put_contents(LOG_FILE, '**CONFIG TIMEZONE: '.$timezone."\n", FILE_APPEND);

if ($timezone) { // set PHP timezone from config
	date_default_timezone_set($timezone);
	file_put_contents(LOG_FILE, '**TIMEZONE SET FROM CONFIG'."\n", FILE_APPEND);
}
else { // trap & fix PHP timezone config warning problem (not an issue in PHP7)
	set_error_handler("tz_error_handler");
	@date_default_timezone_get();
	restore_error_handler();
}

file_put_contents(LOG_FILE, '**PHP VERSION: '.phpversion()."\n", FILE_APPEND);
file_put_contents(LOG_FILE, '**PHP TIMEZONE: '.date_default_timezone_get()."\n", FILE_APPEND);
file_put_contents(LOG_FILE, '**MAC TIMEZONE ABBR: '.exec('date +%Z')."\n", FILE_APPEND);
file_put_contents(LOG_FILE, '**MAC TIMEZONE NAME: '.timezone_name_from_abbr(exec('date +%Z'))."\n", FILE_APPEND);

//??? FEED INFO

$feed_info = array(
	'espncricinfo' =>
		array(
			'name' => 'ESPNcricinfo',
			'site_url' => 'http://www.espncricinfo.com',
			'base_url' => 'http://api.espncricinfo.com',
			'summary_path' => '/netstorage/summary.json'
		),
	'cricbuzz' =>
		array(
			'name' => 'Cricbuzz',
			'site_url' => 'http://www.cricbuzz.com',
			'base_url' => 'http://synd.cricbuzz.com',
			'summary_path' => '/j2me/1.0/livematches.xml'
		)
);

//??? ABBREVIATIONS

$abbrev_map = array();
if ($feed == 'espncricinfo')
	// to fix dodgy country abbreviations (menubar summary only)
	$abbrev_map = array(
		'BDESH' => 'BAN',
		'INDIA' => 'IND',
		'SCOT' => 'SCO',
	);
else if ($feed == 'cricbuzz')
	// Cricbuzz doesn't seem to be keen on providing full names so here's a partial translation
	$abbrev_map = array(
		'AFG' => 'Afganistan',
		'AUS' => 'Australia',
		'BAN' => 'Bangladesh',
		'DERBY' => 'Derbyshire',
		'DUR' => 'Durham',
		'ENG' => 'England',
		'ESS' => 'Essex',
		'GLAM' => 'Glamorgan',
		'GLOUCS' => 'Gloucestershire',
		'HAM' => 'Hampshire',
		'HK' => 'Hong Kong',
		'IND' => 'India',
		'IRE' => 'Ireland',
		'KENT' => 'Kent',
		'KEN' => 'Kenya',
		'LEIC' => 'Leicestershire',
		'LANCS' => 'Lancashire',
		'MDX' => 'Middlesex',
		'NED' => 'Netherlands',
		'NEP' => 'Nepal',
		'NAM' => 'Namibia',
		'NOR' => 'Northamptonshire',
		'NOTTS' => 'Nottinghamshire',
		'NSW' => 'New South Wales',
		'NZ' => 'New Zealand',
		'PAK' => 'Pakistan',
		'PNG' => 'Papua New Guinea',
		'QL' => 'Queensland',
		'RSA' => 'South Africa',
		'SAUS' => 'South Australia',
		'SL' => 'Sri Lanka',
		'SCO' => 'Scotland',
		'SOM' => 'Somerset',
		'SUS' => 'Sussex',
		'SUR' => 'Surrey',
		'TAS' => 'Tasmania',
		'UAE' => 'United Arab Emirates',
		'VIC' => 'Victoria',
		'WA' => 'Western Australia',
		'WARKS' => 'Warwickshire',
		'WI' => 'West Indies',
		'WORCS' => 'Worcestershire',
		'YORKS' => 'Yorkshire',
		'ZIM' => 'Zimbabwe',
	);

//??? SUMMARY FEED

$summary_url = $feed_info[$feed]['base_url'].$feed_info[$feed]['summary_path'];
$summary_feed = curl_obj($summary_url);
file_put_contents(LOG_FILE, "**SUMMARY FEED ($summary_url):\n", FILE_APPEND);
if ($summary_feed)
	file_put_contents(LOG_FILE, print_r($summary_feed, TRUE), FILE_APPEND);
else
	file_put_contents(LOG_FILE, "--EMPTY\n", FILE_APPEND);

//??? CURRENT MATCHES

$matches = array();
if ($summary_feed) {
	if ($feed == 'espncricinfo')
		foreach ($summary_feed->matches as $index => $this_match_obj)
			$matches[$index] = $this_match_obj;
	else if ($feed == 'cricbuzz') {
		foreach ($summary_feed->match as $this_match_obj) {
			$this_match_id = (int)$this_match_obj->attributes()['id'];
			// summary of finished matches lacks some detail, so get try to get detailed instead
			$this_detailed_feed_url = (string)$this_match_obj->attributes()['datapath'].'commentary.xml';
			$this_detailed_feed_obj = curl_obj($this_detailed_feed_url);
			if ($this_detailed_feed_obj)
				$matches[$this_match_id] = $this_detailed_feed_obj->match; // includes batsmen
			else
				$matches[$this_match_id] = $this_match_obj;
		}
	}
}
file_put_contents(LOG_FILE, "**MATCHES ARRAY:\n", FILE_APPEND);
file_put_contents(LOG_FILE, print_r($matches, TRUE), FILE_APPEND);

// retrieve selected match from "current matches" array
$selected_match_obj = NULL;
if ($selected_match_id !== '') {
	if (isset($matches[$selected_match_id]))
		$selected_match_obj = $matches[$selected_match_id];
	else
		file_put_contents(LOG_FILE, "**SELECTED MATCH (ID $selected_match_id) NOT FOUND IN SUMMARY FEED\n", FILE_APPEND);
}
else
	file_put_contents(LOG_FILE, "**NO MATCH SELECTED\n", FILE_APPEND);

//??? MENUBAR -------------------------------------------------------------------------------------------------

if ($selected_match_obj) { // display selected match information

	file_put_contents(LOG_FILE, "**SELECTED MATCH SUMMARY (MATCH ID $selected_match_id):\n", FILE_APPEND);
	file_put_contents(LOG_FILE, print_r($selected_match_obj, TRUE), FILE_APPEND);

	if ($feed == 'espncricinfo') {
		$team1_abbrev = translate_name($selected_match_obj->team1_abbrev);
		$team2_abbrev = translate_name($selected_match_obj->team2_abbrev);
		$team1_score = html_entity_decode($selected_match_obj->team1_score); // lose &amp; &nbsp; etc
		$team1_score = trim(strtok($team1_score, '(')); // lose "(31.4 ov)*" text
		$team2_score = html_entity_decode($selected_match_obj->team2_score);
		$team2_score = trim(strtok($team2_score, '('));
		echo
			strtoupper($team1_abbrev).($team1_score ? ' '.$team1_score : '')
			.' v '
			.strtoupper($team2_abbrev).($team2_score ? ' '.$team2_score : '')
			."\n"
		;
	}

	else if ($feed == 'cricbuzz') {
		$team1_abbrev = (string)$selected_match_obj->Tm[0]->attributes()['sName'];
		$team2_abbrev = (string)$selected_match_obj->Tm[1]->attributes()['sName'];
		$team1_score1 = $team1_score2 = $team2_score1 = $team2_score2 = '';
		$team1_batting = $team2_batting = FALSE;
		if (isset($selected_match_obj->mscr)) { // no scores for games yet to start
			$scores = $overs = array();
			$this_team = (string)$selected_match_obj->mscr->btTm->attributes()['sName'];
			foreach ($selected_match_obj->mscr->btTm->Inngs as $innings_obj) {
				$scores[] =
					(int)$innings_obj->attributes()['r']
					.((int)$innings_obj->attributes()['wkts'] == 10 ? '' : '/'.(int)$innings_obj->attributes()['wkts'])
					.((int)$innings_obj->attributes()['Decl'] ? 'd' : '')
				;
				$overs[] = (string)$innings_obj->attributes()['ovrs'];
			}
			$scores = array_reverse($scores); // force second innings score to be last
			if ($this_team == $team1_abbrev) {
				$team1_batting = TRUE;
				$team1_score1 = (isset($scores[0]) ? $scores[0] : '');
				$team1_score2 = (isset($scores[1]) ? $scores[1] : '');
				$team1_overs = $overs[0];
				file_put_contents(LOG_FILE, "**BATTING TEAM(1): $this_team\n", FILE_APPEND);
				file_put_contents(LOG_FILE, "--team1_score1=$team1_score1, team1_score2=$team1_score2, team1_overs=$team1_overs\n", FILE_APPEND);
			}
			else {
				$team2_batting = TRUE;
				$team2_score1 = (isset($scores[0]) ? $scores[0] : '');
				$team2_score2 = (isset($scores[1]) ? $scores[1] : '');
				$team2_overs = $overs[0];
				file_put_contents(LOG_FILE, "**BATTING TEAM(2): $this_team\n", FILE_APPEND);
				file_put_contents(LOG_FILE, "--team2_score1=$team2_score1, team2_score2=$team2_score2, team2_overs=$team2_overs\n", FILE_APPEND);
			}
			$scores = array();
			$this_team = (string)$selected_match_obj->mscr->blgTm->attributes()['sName'];
			foreach ($selected_match_obj->mscr->blgTm->Inngs as $innings_obj) {
				$scores[] =
					(int)$innings_obj->attributes()['r']
					.((int)$innings_obj->attributes()['wkts'] == 10 ? '' : '/'.(int)$innings_obj->attributes()['wkts'])
					.((int)$innings_obj->attributes()['Decl'] ? 'd' : '')
				;
			}
			$scores = array_reverse($scores); // test match scores have 2nd innings first
			if ($this_team == $team1_abbrev) {
				$team1_score1 = (isset($scores[0]) ? $scores[0] : '');
				$team1_score2 = (isset($scores[1]) ? $scores[1] : '');
				file_put_contents(LOG_FILE, "**BOWLING TEAM(1): $this_team\n", FILE_APPEND);
				file_put_contents(LOG_FILE, "--team1_score1=$team1_score1, team1_score2=$team1_score2\n", FILE_APPEND);
			}
			else {
				$team2_score1 = (isset($scores[0]) ? $scores[0] : '');
				$team2_score2 = (isset($scores[1]) ? $scores[1] : '');
				file_put_contents(LOG_FILE, "**BOWLING TEAM(2): $this_team\n", FILE_APPEND);
				file_put_contents(LOG_FILE, "--team2_score1=$team2_score1, team2_score2=$team2_score2\n", FILE_APPEND);
			}
		}
		echo
			$team1_abbrev.($team1_score1 ? ' '.$team1_score1 : '').($team1_score2 ? ' & '.$team1_score2 : '')
			.' v '
			.$team2_abbrev.($team2_score1 ? ' '.$team2_score1 : '').($team2_score2 ? ' & '.$team2_score2 : '')
			."\n"
		;
	}
}

else { // display icon - 32x32 png, 144 DPI, base64 encoded using "openssl base64 -A -in icon.png" (recommended 36px dimension too big)
	if ($summary_feed) // normal icon
		echo "|image=iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAB3RJTUUH4QMOAi05yhhp7QAAASNJREFUWMPt1ztKBEEUheGvB0UnECcYQXQE3YAyCIK4BVeiiJswMtDcwA2IgbmRGzBxB4IoGijoKL7apEaKtuYBCiZ1oIK6fe65f3d1UvTWPHZwhzJarzhCO/gWcYKHiu8a+5juM8NIn2dTWMNLCIvVxhzOMYMlPKITeT6wgolE/7eKRG0MdbzhCctohLcqQv0sBI+Hwc/RsK7vBheYxCjecT/oC6xjKwQUuMJGAIm1G8C6vlPsBZCuWjjEQoB6wzEO+h3JZeUcS2wnIKvn3cFq5KlhM5F1Wx1Yq+xbCajZyr6Z6KuHFec2ElnNQQDD/id/ppp/VgbIABkgA2SADJABMkAGyAAZoApQ/iJrmPvD56C7YZkITUGVUb1I9BY96j+yvgC5iURRySFvJAAAAABJRU5ErkJggg==\n";
	else // warning icon
		echo "|image=iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAB3RJTUUH4QgYCQ84FFrJ6AAABC9JREFUWMPll01sVFUUgL87P7UzUDqdn6bQob8DLWpTbIeWFrCIDdIC2poYDYq2gI3QWjFYFWJCNWlrIhsNooEoCmI0qBt/Nizc6YYdi5qgIVGwTa0lYNtpZ96Pi7kz8+Y5Zcb+JCae5Oa9uffMO98795xzz4P/uGQD3cCvgG4Yk8AQ4JF6HwG/mXSuAv3pDNjSrGtS5xowIecEMCuvupxTDTq6XJsArOkAxBzzVmC1XB+Rv1cY/jMD3AJqgIh8Ww3IMwCMAV4gX85dydTtRcCwyZ0PmXQqTesh4EGTTo/0TEznptzSJLGkAKiQBozSafr9bIpYedjkwe2m57uAHZkAZCI6QJ7LgRDCGC/G52qZbPl8AVSA3v1B3joe97x9Pg+aD4AN2FNW7KLjiWqO9DVRdXc+QCsQWHIAIRgA/K/0bKJkrQ+mwlx4tw27zVIKHF5qgGpdp3NrYzFd+4MQioCmc0+Fj8Nd9ciiVSdTc0kAjgiB7/PTj6KEIoRmFEIzCqqq0fV0DaVFLoCvjIG6WABhYCuwd+jYNvLzlzP09g+U1Z1kbeMpul76jsAGPwc7agEKBQxm6oVMAdzAp1Xr8tm3Zz0Af96cZnRskuu/32Z0bApmFfqeb6SmqgAdDsgquWgArTabZWVfdwO+ghwZjImUtlhEvN59+eFjAD6gfDEBxKa61ex9phYiaiwbjJkhS5FOSbGLYy9sXtwscDhsfPZ+O8wkttWT54jfe90O0PU4xKHOWtaUuRcP4L03WylYmQOqNKLrlJfkJQ6Pck9iDSj059KzbwM2q2XhAPdvLKKtpSLJADqURFMOIPq2qqH0Kxq9BzdSEfAsDOCuLBt9hxrINbg7JjnLsrDZLAkA7Z9pf+FU28IAdjYH2LWzMvntYnGRbcfndqbcAgAiKtX3raK/r2l+AFaLhXMnH0lpPApgw+OOeibb40wEYVL5UunpDFIZ8P4rgDKAs+/sZpkrO6VrAZwOO2vKPDQE/XNCAni8Tl7ubsAaDchgugbBDfzSsi3g+vr841itYm7f2a3cmphGVTXcq3LRp8JJtcFoIRLWaGo/x4+Xr4eAemN/aPbAeVdututob+OdjQvBmbOXCW7/gM27P6b/9UsIp33O3snusHH6RCuAAxgw2jUCtAOtbTsq2LKlNG0v/cf4ND9fm2D46jg3RibBcgdgRePeWj8vPlePbHBbzABeYNDrdnLieDNoWvrjUVHj97MRde4GPyZTYQaPPkCg1J0FvBFr4YQcrwKDly4+SfOudTCrpAW4PT7FyOgkQoA7z4nXvyJ1wOrJnvv2m2HaOi6iKNprwIAACoGfvG7n8gNPrUfXEqEp4lsuDNsvkg6g6FWkOKBS64VCET754go3Rv8CKAY4Y/i4mJGfXappKHI+LK+KbLuNI6anyHvjWuzjxDz3Pf97+Rtb1UL3NeDX6QAAAABJRU5ErkJggg==\n";
}

//??? SUBMENU -------------------------------------------------------------------------------------------------

// no data
if (!$summary_feed) {
	echo "---\n";
	echo "Unable to get ".$feed_info[$feed]['name']." data\n";
}

// detailed match information
if ($selected_match_obj) {

	echo "---\n";

	//??? DETAILED - ESPNcricinfo

	if ($feed == 'espncricinfo') {
		if (array_key_exists($selected_match_id, $matches)) {

			// get detailed match feed data
			$match_url = $feed_info[$feed]['site_url'].$selected_match_obj->url; // note: site url NOT base url
			$detailed_url = str_replace('.html', '.json', $match_url);
			$detailed_feed = curl_obj($detailed_url);
			file_put_contents(LOG_FILE, "**DETAILED FEED:\n", FILE_APPEND);
			file_put_contents(LOG_FILE, print_r($detailed_feed, TRUE), FILE_APPEND);

			if ($detailed_feed) {

				// output match overview
				echo
					strtoupper($selected_match_obj->team1_name).' v '.strtoupper($selected_match_obj->team2_name) // e.g. "SRI LANKA v BANGLADESH"
					.($detailed_feed->series[0]->match_title ? ' – '.$detailed_feed->series[0]->match_title : '') // e.g. "1st Test"
					.', '.($detailed_feed->match->town_aka ? $detailed_feed->match->town_aka : $detailed_feed->match->town_name) // e.g. "Galle"
					.' | color=black'."\n"
				;

				// option key alternative: display series & match dates, e.g. "West Indies in England ODI Series (Sep 21, 2017)"
				echo
					$detailed_feed->series[0]->series_name
					.(($detailed_feed->series[0]->trophy_name !== $detailed_feed->series[0]->series_name) ? ' – '.$detailed_feed->series[0]->trophy_name : '') // "The Ashes" is both the series & the trophy name
					.' ('.$detailed_feed->match->date.')'
					." | alternate=true \n";

				// team id/team name/score array
				$teams = array();
				$teams[$detailed_feed->match->team1_id] = array('name' => $detailed_feed->match->team1_name, 'score' => '');
				$teams[$detailed_feed->match->team2_id] = array('name' => $detailed_feed->match->team2_name, 'score' => '');
				file_put_contents(LOG_FILE, "**TEAMS ARRAY:\n", FILE_APPEND);
				file_put_contents(LOG_FILE, print_r($teams, TRUE), FILE_APPEND);

				if (isset($detailed_feed->match->home_team_id)) {
					$home_team_id = $detailed_feed->match->home_team_id;
					file_put_contents(LOG_FILE, "**HOME_TEAM_ID (SUPPLIED) = $home_team_id\n", FILE_APPEND);
				}
				else {
					$home_team_id = $detailed_feed->match->team1_id;
					file_put_contents(LOG_FILE, "**HOME_TEAM_ID (TEAM1) = $home_team_id\n", FILE_APPEND);
				}
				if (isset($detailed_feed->match->away_team_id)) {
					$away_team_id = $detailed_feed->match->away_team_id;
					file_put_contents(LOG_FILE, "**AWAY_TEAM_ID (SUPPLIED) = $away_team_id\n", FILE_APPEND);
				}
				else {
					$away_team_id = $detailed_feed->match->team2_id;
					file_put_contents(LOG_FILE, "**AWAY_TEAM_ID (TEAM2) = $away_team_id\n", FILE_APPEND);
				}

				// make sure home team listed first
				reset($teams);
				$first_team_id = key($teams);
				if ($home_team_id != $first_team_id)
					$teams = array_reverse($teams, TRUE);

				// get innings data
				$match_started = FALSE;
				$winner = $detailed_feed->match->winner_team_id;
				file_put_contents(LOG_FILE, "**WINNER: ".$winner."\n", FILE_APPEND);
				if ($detailed_feed->innings) {
					$match_started = TRUE;
					foreach ($detailed_feed->innings as $innings) {

						file_put_contents(LOG_FILE, "**INNINGS:\n", FILE_APPEND);
						file_put_contents(LOG_FILE, print_r($innings, TRUE), FILE_APPEND);

						if ($teams[$innings->batting_team_id]['score'])
							// if team's second innings add '&'
							$teams[$innings->batting_team_id]['score'] .= ' & ';
						$innings_score = $innings->runs.'/'.$innings->wickets;
						if ($innings->live_current && ($innings->event_name != 'complete') && !$winner) {
							$innings_score .= ' ('.$innings->overs.' ov)';
							$innings_score .= ' CRR '.$detailed_feed->live->innings->run_rate;
							file_put_contents(LOG_FILE, "**TARGET: ".$detailed_feed->live->innings->target."\n", FILE_APPEND);
						}
						$teams[$innings->batting_team_id]['score'] .= $innings_score;
						$teams[$innings->batting_team_id]['batted'] = $innings->batted; // make a note if team has batted yet

						file_put_contents(LOG_FILE, "**OVER_LIMIT ".$innings->over_limit."\n", FILE_APPEND);
						file_put_contents(LOG_FILE, "**TEAM DATA (TEAM ID ".$innings->batting_team_id."):\n", FILE_APPEND);
						file_put_contents(LOG_FILE, print_r($teams[$innings->batting_team_id], TRUE), FILE_APPEND);

						if ($innings->live_current && !$winner) {
							// required run rate
							$required_runrate = $detailed_feed->live->innings->required_run_rate;
							if ($required_runrate)
								$teams[$innings->batting_team_id]['score'] .= ' RRR '.$required_runrate;
							// projected score
							if (($innings->event_name != 'complete') && ($innings->event_name != 'all out')) {
								$noofovers = (int)$innings->over_limit;
								if ((int)$innings->ball_limit && ($innings->overs > $noofovers / 2))
									$teams[$innings->batting_team_id]['score'] .= projected_score($noofovers, $innings->overs, $innings->runs);
							}
						}

					}
				}

				// get names/scores of batsmen at crease
				$batsmen = array();
				foreach ($detailed_feed->live->batting as $batsman) {
					$batsman_name = get_batsman($detailed_feed,$batsman->player_id);
					$batsman_score = $batsman->runs . ' (' . $batsman->balls_faced . ')';
					if ($batsman->live_current_name == 'striker')
						$batsman_name .= '*';
					$batsmen[] = $batsman_name . ': ' . $batsman_score;
				}
				file_put_contents(LOG_FILE, "**BATSMEN:\n", FILE_APPEND);
				file_put_contents(LOG_FILE, print_r($batsmen, TRUE), FILE_APPEND);

				// the match status
				$match_status = $detailed_feed->live->status;
				file_put_contents(LOG_FILE, "**MATCH STATUS: ".$match_status."\n", FILE_APPEND);
				echo "$match_status... | href=$match_url \n"; // e.g. "Day 1: South Africa elected to bat first" which links to Cricinfo website

				// neutral ground?
				$neutral_ground = FALSE;
				if (isset($detailed_feed->match->neutral_match))
					$neutral_ground = $detailed_feed->match->neutral_match;

				// work out match type
				$limited_overs = $detailed_feed->match->scheduled_overs;
				$type_desc = '';
				if (strpos($detailed_feed->match->international_class_card,'Test') !== FALSE) // "Test" or "Women's Test"
					$type_desc .= 'Test match, '.$detailed_feed->match->scheduled_days.' days';
				else if (strpos($detailed_feed->match->international_class_card,'ODI') !== FALSE)
					$type_desc = 'ODI';
				else if (strpos($detailed_feed->match->international_class_card,'Twenty20') !== FALSE)
					$type_desc = 'T20 international';
				else if ($detailed_feed->match->general_class_card) { // county matches, some T20 etc
					if (strpos($detailed_feed->match->general_class_name,'Twenty20') !== FALSE)
						$type_desc .= 'T20';
					else {
						$type_desc = $detailed_feed->match->general_class_card.' match, '.$detailed_feed->match->scheduled_days.' day'.($detailed_feed->match->scheduled_days > 1 ? 's' : '');
						if ($detailed_feed->match->scheduled_overs)
							$type_desc .= ', '.$detailed_feed->match->scheduled_overs.' overs';
					}
				}
				else if (strpos($detailed_feed->match->general_class_name,'Twenty20') !== FALSE) // e.g. "Other Twenty20 matches"
						$type_desc .= 'T20';
				file_put_contents(LOG_FILE, "**MATCH TYPE: $type_desc (international_class_card=".$detailed_feed->match->international_class_card.", general_class_card=".$detailed_feed->match->general_class_card.", general_class_name=".$detailed_feed->match->general_class_name.', scheduled days='.$detailed_feed->match->scheduled_days.', scheduled overs='.$detailed_feed->match->scheduled_overs.")\n", FILE_APPEND);

				// option key alternative - display ground name, country & match type
				echo $detailed_feed->match->ground_name.', '.$detailed_feed->match->country_name.($neutral_ground ? ' - neutral ground' : '').($type_desc ? " ($type_desc)" : '')." | alternate=true \n"; // e.g. "P Sara Oval, Columbo, Sri Lanka"

				// match live state (not really usable for "innings in progress" - seems to be free text, i.e. no standardised values)
				$known_live_state = array('Drinks', 'Lunch', 'Innings break', 'Match delayed by a wet outfield', 'Match delayed by rain', 'Match delayed - Rain', 'Match start delayed (No play before Lunch, wet outfield)', 'Match delayed - Wet outfield', 'Stumps'); // ALSO SOMETIMES NOT SET (= MATCH NOT STARTED/ENDED/IN PLAY?)
				$live_state == NULL;
				if (isset($selected_match_obj->live_state)) {
					$live_state = $selected_match_obj->live_state;
					echo $live_state;
					if ($match_started) {
						// get kickoff time
						$next_timestamp = strtotime($detailed_feed->match->next_datetime_gmt.' UTC'); // may take a while to be updated after end of day's play
						file_put_contents(LOG_FILE, "**NEXT_DATETIME_GMT: ".$detailed_feed->match->next_datetime_gmt."\n", FILE_APPEND);
						file_put_contents(LOG_FILE, "**NEXT_TIMESTAMP: ".$next_timestamp.' '.date('Y-m-d H:i',$next_timestamp)."\n", FILE_APPEND);
						file_put_contents(LOG_FILE, "**TIME(): ".time()."\n", FILE_APPEND);
						if ($next_timestamp > time())  // still waiting for kickoff (according to published next start time)
							echo " (resumes ".date("l, H:i T", $next_timestamp).")";
					}
					echo "\n";
				}

				// match yet to begin so provide local start date/time
				if (strpos($match_status, 'Match scheduled to begin') === 0) { // add local time translation
					// translate start date/time to local time
					$start_datetime_gmt = $detailed_feed->match->start_datetime_gmt;
					file_put_contents(LOG_FILE, "**START_DATETIME_GMT: ".$detailed_feed->match->start_datetime_gmt."\n", FILE_APPEND);
					$start_timestamp = strtotime($detailed_feed->match->start_datetime_gmt.' UTC');
					file_put_contents(LOG_FILE, "**START_TIMESTAMP: ".$start_timestamp.' '.date('Y-m-d H:i',$start_timestamp)."\n", FILE_APPEND);
					$start_local = date("jS M g:ia T", $start_timestamp);
					echo "Scheduled start $start_local\n";
				}
				else // toss done, use countdown timer instead
					if (isset($selected_match_obj->match_clock))
						if (!$detailed_feed->innings) // countdown can linger for a while after play starts
							echo "Starting in ".$selected_match_obj->match_clock."\n";

				// show teams and scores
				if ($match_started) {
					foreach ($teams as $team_id => $team_info) {
						echo $team_info['name'] .': ';
						if ($team_info['batted']) {
							$score = str_replace('/10', '', $team_info['score']); // lose wicket count if 10 (i.e. all out)
							if ($winner)
								$score = str_replace('*', '', $score); // lose "currently batting" asterisk if match over
							echo $score;
						}
						else
							echo 'yet to bat';
						echo " | color=black size=12 \n";
					}
				}

				// innings still in progress and not innings break?
				if (!$winner && ($detailed_feed->match->live_state != 'Innings break')) {
					// show batsmen
					foreach ($batsmen as $batsman)
						echo "$batsman | size=12 \n";

					// last wicket
					$fow = $detailed_feed->live->fow;
					foreach ($fow as $item) {
						if ($item->live_current_name == 'current partnership') {
							echo
								'Partnership: '
								.$item->partnership_runs
								.' ('
								.$item->partnership_overs
								.' ov) '
							;
						}
						if ($item->live_current_name == 'last wicket') {
							echo
								'Last wicket: '
								.get_batsman($detailed_feed,$item->out_player->player_id)
								.' '
								.html_entity_decode($item->out_player->dismissal_string)
								.' '
								.$item->out_player->runs
								." | size=12"
							;
							file_put_contents(LOG_FILE, "**LAST WICKET (PLAYER ID = ".$item->out_player->player_id."):\n", FILE_APPEND);
							file_put_contents(LOG_FILE, print_r($item->out_player, TRUE), FILE_APPEND);
						}
					}
					if ($fow)
						echo "\n";
				}
			}
			else
				echo "Detailed feed not available... | href=$match_url \n";

			// debuggery
			file_put_contents(LOG_FILE, "**URL: $detailed_url\n", FILE_APPEND);
			file_put_contents(LOG_FILE, '**MATCH STARTED: '.($match_started ? 'YES' : 'NO')."\n", FILE_APPEND);
			file_put_contents(LOG_FILE, "**LIVE_STATE: ".($live_state === NULL ? 'NOT SET' : $live_state)."\n", FILE_APPEND);
			if ($winner) file_put_contents(LOG_FILE, "**WINNER TEAM ID: $winner\n", FILE_APPEND);

		}
		else // else selected match not found
			file_put_contents(LOG_FILE, "**SELECTED MATCH NOT FOUND IN DETAILED FEED: ".$selected_match_id."\n", FILE_APPEND);
	}

	//??? DETAILED - CRICBUZZ

	else if ($feed == "cricbuzz") {
		// output match overview
		if (isset($selected_match_obj->Tm)) { // these are generally abbreviated names anyway!
			$team1_name = $selected_match_obj->Tm[0]->attributes()['Name'];
			$team2_name = $selected_match_obj->Tm[1]->attributes()['Name'];
			file_put_contents(LOG_FILE, "**TEAM NAMES FROM ->Tm: team1_name=$team1_name, team2_name=$team2_name\n", FILE_APPEND);
		}
		else {
			$team_names = explode(' vs ', (string)$selected_match_obj->attributes()['mchDesc']); // e.g. "Ess vs WI"
			$team1_name = $team_names[0];
			$team2_name = $team_names[1];
			file_put_contents(LOG_FILE, "**TEAM NAMES FROM ->mchDesc: team1_name=$team1_name, team2_name=$team2_name\n", FILE_APPEND);
		}
		$description = strtoupper(translate_name($team1_name)).' v '.strtoupper(translate_name($team2_name)); // translate abbreviated team names & use "v" instead of "vs"
		echo
			$description // e.g. "Essex vs West Indies"
			.' – '.(string)$selected_match_obj->attributes()['mnum'] // e.g. "1st 3-day practice match"
			.', '.(string)$selected_match_obj->attributes()['vcity'] // e.g. Chelmsford
			.' | color=black size=14'
			."\n"
		;

		if (isset($selected_match_obj->Tme)) { // note ->Tme may not exist: e.g. mchState=nextlive, status="Coming up on Sep 15 at 13:30 GMT"
			// option key alternative - display match tour & dates, e.g. "West Indies tour of England, 2017; 1st August - 3rd August"
			$start_date = date("jS F", strtotime((string)$selected_match_obj->Tme->attributes()['Dt']));
			$end_date = date("jS F", strtotime((string)$selected_match_obj->Tme->attributes()['enddt']));
			echo
				(string)$selected_match_obj->attributes()['srs']
				.' ('
				.$start_date
				.($start_date != $end_date ? " – $end_date" : '')
				.')'
				." | alternate=true \n"
			;

			// local start date/time
			$start_gmt = (string)$selected_match_obj->Tme->attributes()['Dt'].' '.(string)$selected_match_obj->Tme->attributes()['stTme'].' GMT';
			$start_local = date("jS M g:ia T", strtotime($start_gmt));
		}

		// output match status
		$status_message = array();
		$status = (string)$selected_match_obj->state->attributes()['status']; // e.g. "Day 1: Stumps", "Ess trail by 291 runs", "No result"
		$status_message[] = translate_names($status); // translate abbreviated team names
		if ((string)$selected_match_obj->state->attributes()['addnStatus'])
			$status_message[] = (string)$selected_match_obj->state->attributes()['addnStatus']; // e.g. "Day 2: Rain stops play", "No result"
		if (strpos($status_message[1], 'Day ') !== FALSE) // sometimes "Day x: ..." message is in 'addnStatus'
			$status_message = array_reverse($status_message);
		$status_message = array_unique($status_message); // to get rid of duplicate messages, e.g. "Day 1: 2nd Session", "No result"
		echo implode(' - ', $status_message)." | color=black size=14 \n";

		// work out if innings in progress
		// mchState values... NOTE: still 'lunch' & 'tea' for day/night tests, i.e. no 'dinner'
		//???CHECK WHAT HAPPENS IF INNINGS BREAK & LUNCH/TEA COINCIDE
		$known_mchState = array('nextlive', 'preview', 'delay', 'inprogress', 'lunch', 'tea', 'stump', 'innings break', 'rain', 'wetoutfield', 'badlight', 'unforeseen', 'complete', 'Result', 'abandon');
		$match_state = (string)$selected_match_obj->state->attributes()['mchState'];
		$in_progress = (array_search($match_state, array('inprogress', 'lunch', 'tea', 'stump', 'rain', 'badlight', 'wetoutfield', 'unforeseen')) !== FALSE);
		file_put_contents(LOG_FILE, "**MATCH STATE: ".$match_state." (".($in_progress ? '' : 'NOT ')."IN PROGRESS)\n", FILE_APPEND);

// record unknown match states
if (array_search($match_state, $known_mchState) === FALSE)
	file_put_contents(DEBUG_FILE, $match_state."\n", FILE_APPEND);

		// match type: "TEST", "ODI", "T20"
		$type = (string)$selected_match_obj->attributes()['type'];
		$limited_overs = (($type == 'ODI') || ($type == 'T20'));
		$noofovers = NULL;
		if ($limited_overs) { // note: mscr->inngsdetail->attributes()['noofovers'] can be incorrect (e.g. 50 given for T20 match)
			$noofovers = 50;
			if ($type == 'T20')
				$noofovers = 20;
		}
		file_put_contents(LOG_FILE, "**MATCH TYPE: $type\n", FILE_APPEND);
		file_put_contents(LOG_FILE, "**LIMITED OVERS: $limited_overs\n", FILE_APPEND);
		if (isset($selected_match_obj->mscr->inngsdetail))
			file_put_contents(LOG_FILE, "**NO OF OVERS (SUPPLIED): ".$selected_match_obj->mscr->inngsdetail->attributes()['noofovers']."\n", FILE_APPEND);
		file_put_contents(LOG_FILE, "**NO OF OVERS (DEDUCED): $noofovers\n", FILE_APPEND);

		// option key alternative - display ground name, country & match type
		echo
			(string)$selected_match_obj->attributes()['grnd'] // e.g. "County Ground"
			.', '.(string)$selected_match_obj->attributes()['vcity'] // e.g. "Chelmsford"
			.', '.(string)$selected_match_obj->attributes()['vcountry'] // e.g. "England"
			.($type ? " ($type)" : '')
			." | alternate=true \n"
		;

		if (strpos($status, 'Starts on') === 0) // add local start time
			echo "Scheduled start $start_local\n";

		// if match has started (or ended) show teams & scores, checking that there's some scores to look at (plus if innings in progress: overs, CRR, RRR, Proj Scr)
		if (((string)$selected_match_obj->state->attributes()['mchState'] != 'preview') && isset($selected_match_obj->mscr)) {
			if (isset($selected_match_obj->mscr->inngsdetail->attributes()['crr'])) {
				$ov_crr_rrr = '';
				if ($in_progress) {
					$ov_crr_rrr = ' CRR '.(string)$selected_match_obj->mscr->inngsdetail->attributes()['crr'];
					if ((int)$selected_match_obj->mscr->inngsdetail->attributes()['rrr'])
						$ov_crr_rrr .= ' RRR '.(string)$selected_match_obj->mscr->inngsdetail->attributes()['rrr'];
					if ($team1_batting) {
						$overs = " ($team1_overs ov)";
						if ($team1_score2)
							$team1_score2 .= $overs.$ov_crr_rrr;
						else
							$team1_score1 .= $overs.$ov_crr_rrr;
						// projected score
						if ($limited_overs && ((int)$team1_overs > ($noofovers / 2)))
							$team1_score1 .= projected_score($noofovers, $team1_overs, $team1_score1);
					}
					else if ($team2_batting) {
						$overs = " ($team2_overs ov)";
						if ($team2_score2)
							$team2_score2 .= $overs.$ov_crr_rrr;
						else
							$team2_score1 .= $overs.$ov_crr_rrr;
						// projected score
						if ($limited_overs && ((int)$team2_overs > ($noofovers / 2)))
							$team2_score1 .= projected_score($noofovers, $team2_overs, $team2_score1);
					}
				}
			}
			echo
				translate_name($team1_abbrev)
				.": "
				.($team1_score1 ?  $team1_score1 : 'yet to bat')
				.($team1_score2 ? ' & ' . $team1_score2 : '')
				.' | color=black size=12'
				."\n"
			;
			echo
				translate_name($team2_abbrev)
				.": "
				.($team2_score1 ?  $team2_score1 : 'yet to bat')
				.($team2_score2 ? ' & ' . $team2_score2 : '')
				.' | color=black size=12'
				." \n"
			;
		}

		// show batsmen, scores, current partnership, last wicket (if innings still in progress)
		if ($in_progress) {
			if (isset($selected_match_obj->mscr->btsmn)) { // not available if datapath (& hence detailed feed) not set
				foreach ($selected_match_obj->mscr->btsmn as $batsman) // works for single obj or array of obj
					echo
						(string)$batsman->attributes()['sName']
						.": "
						.(string)$batsman->attributes()['r']
						.' ('
						.(string)$batsman->attributes()['b']
						.')'
						." | size=12 \n"
					;
				$partnership = (string)$selected_match_obj->mscr->inngsdetail->attributes()['cprtshp'];
				if ($partnership)
					echo
						'Partnership: '
						.str_replace('(', ' (', (string)$selected_match_obj->mscr->inngsdetail->attributes()['cprtshp'])
						.' '
					;
				// get current scoreboard
				$selected_scorecard_url = (string)$selected_match_obj->attributes()['datapath'].'scorecard.xml';
				$selected_scorecard_obj = curl_obj($selected_scorecard_url);
				file_put_contents(LOG_FILE, "**SCORECARD:\n", FILE_APPEND);
				file_put_contents(LOG_FILE, print_r($selected_scorecard_obj, TRUE), FILE_APPEND);
				// get current innings
				$current_innings = $selected_scorecard_obj->scrs->Inngs[0]; // current innings comes first
				file_put_contents(LOG_FILE, "**CURRENT INNINGS:\n", FILE_APPEND);
				file_put_contents(LOG_FILE, print_r($current_innings, TRUE), FILE_APPEND);
				file_put_contents(LOG_FILE, "**TARGET: ".$current_innings->attributes()['Target']."\n", FILE_APPEND);
				// look at fall of wickets
				$last_wicket= '';
				foreach ($current_innings->FOW->wkt as $wicket) // go through wickets & get last one
					$last_wicket = $wicket;
				file_put_contents(LOG_FILE, "**LAST WICKET:\n", FILE_APPEND);
				file_put_contents(LOG_FILE, print_r($last_wicket, TRUE), FILE_APPEND);
				if ($last_wicket) {
					$last_wicket_player_id = str_replace(',', '', (string)$last_wicket->attributes()['id']); // the ID here is formatted 9,585 !!!!
					$last_wicket_player = (string)$last_wicket->attributes()['btsmn'];
					echo 'Last wicket: '.$last_wicket_player;
					file_put_contents(LOG_FILE, "**LAST WICKET PLAYER ID: $last_wicket_player_id\n", FILE_APPEND);
					// get last wicket's runs & balls
					foreach ($current_innings->btTm->plyr as $player) {
						if ((string)$player->attributes()['Id'] == $last_wicket_player_id) // the ID here is formatted 9585
							echo ' '.(int)$player->attributes()['r'].' ('.(int)$player->attributes()['b'].')';
					}
				}
				if ($partnership || $last_wicket)
					echo " | size=12 \n";
			}
		}
	}
}
//else no match selected

//??? REFRESH AREA

if ($selected_match_obj) {
	echo "---\n";
	echo
		'Refresh now'
		." | terminal=false bash=\"".$argv[0] . "\" refresh=true"
		."\n"
	;
}

//??? CURRENT MATCHES/MATCH SELECTION AREA

if ($summary_feed) {
	echo "---\n";
	echo "Current matches\n";
	foreach ($matches as $index => $this_match_obj) {
		$description = '';
		if ($feed == 'espncricinfo')
			$description = $this_match_obj->team1_name .' v ' .$this_match_obj->team2_name;
		else if ($feed == 'cricbuzz') {
			if (isset($this_match_obj->Tm)) { // these are generally abbreviated names anyway!
				$team1_name = $this_match_obj->Tm[0]->attributes()['Name'];
				$team2_name = $this_match_obj->Tm[1]->attributes()['Name'];
			}
			else {
				$team_names = explode(' vs ', (string)$this_match_obj->attributes()['mchDesc']); // e.g. "Ess vs WI"
				$team1_name = $team_names[0];
				$team2_name = $team_names[1];
			}
			$description = translate_name($team1_name).' v '.translate_name($team2_name); // translate abbreviated team names & use "v" instead of "vs"
		}
		// sub-submenu item
		echo
			'--'
			.$description
			.($index === (int)$selected_match_id ? ' ✓' : '')
			." | terminal=false bash=\"".$argv[0] . "\" param1=\"$index\" refresh=true" // run this PHP script with one argument (match ID)
			."\n"
		;
	}
	if (!count($matches))
		echo "-- none\n";
}

// "clear match selection" menu option
if ($selected_match_obj) {
	echo
		'Clear match selection'
		." | terminal=false bash=\"".$argv[0] . "\" param1=unknown refresh=true" // reset match ID
		."\n"
	;
}

//??? FEED AREA

echo "---\n";
// "link to site" menu item
echo $feed_info[$feed]['name']." website... | href=".$feed_info[$feed]['site_url']."\n";

// "change feed" submenu
echo "Change feed\n";
foreach (array('espncricinfo', 'cricbuzz') as $this_feed) {
	echo
		'--' // sub-submenu item
		.$feed_info[$this_feed]['name']
		.($this_feed == $feed ? ' ✓' : '')
		." | terminal=false bash=\"".$argv[0] . "\" param1=filler param2=\"$this_feed\" refresh=true" // run this PHP script with two arguments (second is feed name)
		."\n"
	;
}

//??? TIMEZONE AREA

echo "---\n";

// make up list of timezones of format "blah/blahblah"
$timezones = array();
foreach (timezone_abbreviations_list() as $abbr => $tz)
	foreach ($tz as $val)
		if (isset($val['timezone_id']))
			if (strpos($val['timezone_id'],'/') !== FALSE)
				$timezones[] = $val['timezone_id'];
// sort and strip duplicates
sort($timezones);
$timezones = array_unique($timezones);

// "set timezone" submenu
echo "Your timezone (".($timezone ? $timezone : date_default_timezone_get()).")\n";
foreach ($timezones as $this_timezone) {
	echo
		'--' // sub-submenu item
		.$this_timezone
		.($this_timezone == $timezone ? ' ✓' : '')
		." | terminal=false bash=\"".$argv[0] . "\" param1=filler param2=filler param3=\"$this_timezone\" refresh=true" // run this PHP script with three arguments (third is timezone name)
		."\n"
	;
}

?>
