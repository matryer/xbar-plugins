#!/usr/bin/env php

<?php

/**
 * worldcup - BitBar WorldCup 2022 scores
 *
 * PHP version 7
 *
 * @author   Daniel Goldsmith <dgold@ascraeus.org>
 * @license  https://opensource.org/licenses/FPL-1.0.0 0BSD
 * @link     https://github.com/dg01d/bitbar-worldcup
 * @category Utility
 * @version  3.1
 * <xbar.title>World Cup 2018</xbar.title>
 * <xbar.version>v1.0</xbar.version>
 * <xbar.author>Daniel Goldsmith</xbar.author>
 * <xbar.author.github>dg01d</xbar.author.github>
 * <xbar.desc>Shows current and daily scores from the 2018 World Cup. Needs Steve Edson's bitbar-php: https://github.com/SteveEdson/bitbar-php </xbar.desc>
 * <xbar.image>https://raw.githubusercontent.com/dg01d/bitbar-worldcup/master/bitbar-worldcup.png</xbar.image>
 * <xbar.dependencies>php,bitbar-php</xbar.dependencies>
 * <xbar.abouturl>https://github.com/dg01d/bitbar-worldcup</xbar.abouturl>
 * Instructions: Install bitbar-php following the instructions on that project's github page.
 * Uses the wonderful World Cup API provided by http://worldcupjson.ney
 */

require ".bitbar/vendor/autoload.php";

use SteveEdson\BitBar;

function array_msort($array, $cols)
{
    $colarr = array();
    foreach ($cols as $col => $order) {
        $colarr[$col] = array();
        foreach ($array as $k => $row) { $colarr[$col]['_'.$k] = strtolower($row[$col]); }
    }
    $eval = 'array_multisort(';
    foreach ($cols as $col => $order) {
        $eval .= '$colarr[\''.$col.'\'],'.$order.',';
    }
    $eval = substr($eval,0,-1).');';
    eval($eval);
    $ret = array();
    foreach ($colarr as $col => $arr) {
        foreach ($arr as $k => $v) {
            $k = substr($k,1);
            if (!isset($ret[$k])) $ret[$k] = $array[$k];
            $ret[$k][$col] = $array[$k][$col];
        }
    }
    return $ret;
}


$flagsrc = '{"SEN":"🇸🇳","NED":"🇳🇱","QAT":"🇶🇦","ECU":"🇪🇨","ENG":"🏴󠁧󠁢󠁥󠁮󠁧󠁿","IRN":"🇮🇷","WAL":"🏴󠁧󠁢󠁷󠁬󠁳󠁿","USA":"🇺🇸","ARG":"🇦🇷","KSA":"🇸🇦","MEX":"🇲🇽","POL":"🇵🇱","FRA":"🇫🇷","DEN":"🇩🇰","TUN":"🇹🇳","AUS":"🇦🇺",
"ESP":"🇪🇸","GER":"🇩🇪","JPN":"🇯🇵","CRC":"🇨🇷","BEL":"🇧🇪","CAN":"🇨🇦","MAR":"🇲🇦","CRO":"🇭🇷","BRA":"🇧🇷","SRB":"🇷🇸","SUI":"🇨🇭","CMR":"🇨🇲","POR":"🇵🇹","GHA":"🇬🇭","URU":"🇺🇾","KOR":"🇰🇷"}';

$flags = json_decode($flagsrc, true);

// Create BitBar formatter
$bb = new BitBar();

$json = file_get_contents("http://worldcupjson.net/matches/current");
$data = json_decode($json, true);


if (!empty($data)) {
    $homeTeam = $data[0]['home_team']['country'];
    $homeTeamFlag= $flags[$homeTeam];
    $homeTeamScore = $data[0]['home_team']['goals'];
    $awayTeam = $data[0]['away_team']['country'];
    $awayTeamFlag = $flags[$awayTeam];
    $awayTeamScore = $data[0]['away_team']['goals'];
    $scoreLine = "$homeTeamFlag $homeTeamScore — $awayTeamScore $awayTeamFlag";
} else {
    $scoreLine = "⚽";
};

$line = $bb->newLine();

$line
    ->setText($scoreLine)
    ->setFontFace("SF Mono")
    ->show();

$todayJson = file_get_contents("http://worldcupjson.net/matches/today");
$todayData = json_decode($todayJson, true);

if (!empty($todayData)) {
    $cnt = count($todayData);
    for ($n = 0; $n < $cnt; $n++) {
        $team1 = $todayData[$n]['home_team']['name'];
        $team1code =  $todayData[$n]['home_team']['country'];
        $team1flag = $flags[$team1code];
        $team1s = $todayData[$n]['home_team']['goals'];
        $team2 = $todayData[$n]['away_team']['name'];
        $team2code =  $todayData[$n]['away_team']['country'];
        $team2flag = $flags[$team2code];
        $team2s = $todayData[$n]['away_team']['goals'];
        $scores = "$team1code $team1flag $team1s – $team2s $team2flag $team2code";
       // $match = "\"https://www.fifa.com/worldcup/matches/match/" . $todayData[$n]['fifa_id'] . "/#match-summary\"";
        if (($todayData[$n]['status']) == "in progress") {
            $time = $todayData[$n]['time'];
            $scores = $scores . " " . $time . " ⚽| href=$match";
        } else {
            $scores .= "";  //| href=$match";
        }
        if (($todayData[$n]['status'] == "completed") || ($todayData[$n]['status'] == "in progress")) {
            $line = $bb->newLine();

            $arrayEvents = array_merge($todayData[$n]['home_team_events'], $todayData[$n]['away_team_events']);
            $arraySortEvents = array_msort($arrayEvents, array('id'=>SORT_ASC));
            foreach ($arraySortEvents as $val) {
                if (in_array($val['type_of_event'], array('goal', "goal-own", "goal-penalty"))) {
                    $scores .= "\n\033[35m";
                    $scores .= $val['player'] . " " . $val['time'] . "| size=11 ";
                }
                if ($val['type_of_event'] == "goal-penalty") {
                    $scores .= " (P)";
                }
                if ($val['type_of_event'] == "goal-own") {
                    $scores .= " (OG)";
                }
                if (in_array($val['type_of_event'], array('red-card', "booking"))) {
                    $scores .= "\n\033[35m";
                    $scores .= $val['player'] . " " . $val['time'];
                
                    if ($val['type_of_event'] == "booking") {
                        $scores .= " 🟨";
                    }
                    if ($val['type_of_event'] == "red-card") {
                        $scores .= " 🟥";
                    }
                    $scores .= " | size=11 ";
                    
                }}

            $comGame = $line
                ->setText($scores)
                ->setDropdown(true);
            $comGame->show();
        } else {
            $line = $bb->newLine();
            $line
                ->setText($scores)
                ->setFontFace("SF Mono")
                ->setDropdown(true)
                ->show();
        }
    }
}
