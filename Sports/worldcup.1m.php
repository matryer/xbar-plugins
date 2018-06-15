#!/usr/bin/php

<?php

/**
 * worldcup - BitBar WorldCup 2018 scores
 *
 * PHP version 7
 *
 * @author   Daniel Goldsmith <dgold@ascraeus.org>
 * @license  https://opensource.org/licenses/FPL-1.0.0 0BSD
 * @link     https://github.com/dg01d/worldcup-bitbar
 * @category Utility
 * @version  1.0
 * <bitbar.title>World Cup 2018</bitbar.title>
 * <bitbar.version>v1.0</bitbar.version>
 * <bitbar.author>Daniel Goldsmith</bitbar.author>
 * <bitbar.author.github>dg01d</bitbar.author.github>
 * <bitbar.desc>Shows current and daily scores from the 2018 World Cup. Needs Steve Edson's bitbar-php: https://github.com/SteveEdson/bitbar-php </bitbar.desc>
 * <bitbar.image>https://raw.githubusercontent.com/dg01d/bitbar-worldcup/master/bitbar-worldcup.png</bitbar.image>
 * <bitbar.dependencies>php,bitbar-php</bitbar.dependencies>
 * <bitbar.abouturl>https://github.com/dg01d/bitbar-worldcup</bitbar.abouturl>
 * Instructions: Install bitbar-php following the instructions on that project's github page.
 * Uses the wonderful World Cup API provided by http://worldcup.sfg.io
 */

require ".bitbar/vendor/autoload.php";

use SteveEdson\BitBar;

// Create BitBar formatter
$bb = new BitBar();

$json = file_get_contents("http://worldcup.sfg.io/matches/current");
$data = json_decode($json, true);

if (!empty($data)) {
    $homeTeam = $data[0]['home_team']['code'];
    $homeTeamScore = $data[0]['home_team']['goals'];
    $awayTeam = $data[0]['away_team']['code'];
    $awayTeamScore = $data[0]['away_team']['goals'];
    $scoreLine = "$homeTeam $homeTeamScore : $awayTeamScore $awayTeam";
} else {
    $scoreLine = "⚽";
};

$line = $bb->newLine();

$line
    ->setText($scoreLine)
    ->show();

$todayJson = file_get_contents("http://worldcup.sfg.io/matches/today");
$todayData = json_decode($todayJson, true);

if (!empty($todayData)) {
    $cnt = count($todayData);
    for ($n = 0; $n < $cnt; $n++) {
        $team1 = $todayData[$n]['home_team']['country'];
        $team1s = $todayData[$n]['home_team']['goals'];
        $team2 = $todayData[$n]['away_team']['country'];
        $team2s = $todayData[$n]['away_team']['goals'];
        $scores = "$team1 $team1s : $team2s $team2";
        if (($todayData[$n]['status']) == "in progress") {
            $time = $todayData[$n]['time'];
            $scores = $scores . " " . $time . " ⚽";
        }
        if (($todayData[$n]['status']) == "completed") {
            $line = $bb->newLine();
            $line
                ->setText($scores)
                ->setDropdown(true)
                ->setColor("blue")
                ->show();
        } else {
            $line = $bb->newLine();
            $line
                ->setText($scores)
                ->setDropdown(true)
                ->show();
        }
    }
}
