#!/usr/bin/php

<?php

# <bitbar.title>MenuMetar</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Andrew Larson</bitbar.author>
# <bitbar.author.github>htmlarson</bitbar.author.github>
# <bitbar.desc>A simple METAR reporter with direct links to the DarkSky or NWS report.</bitbar.desc>
# <bitbar.dependencies>php</bitbar.dependencies>


/*
To configure this plugin or if you desire multiple reports, use this syntax:

{ICAO}.metar.10m.php

For example, for KJFK:

KJFK.MenuMETAR.5m.php

*/

$filename_array = explode(".", basename(__FILE__));
$ICAO = strtoupper($filename_array[0]);


$metar = file_get_contents("https://www.aviationweather.gov/adds/dataserver_current/httpparam?datasource=metars&requestType=retrieve&format=xml&mostRecentForEachStation=constraint&hoursBeforeNow=1.25&stationString=" . $ICAO) or die($ICAO . ": No data");

$metar_object = new SimpleXMLElement($metar);
$ready_to_parse = $metar_object->data->METAR;

$skycond_setup = $ready_to_parse->sky_condition->attributes();
$skycond_ready = $skycond_setup[0];
if($skycond_setup[1]) {
  $skycond_ready .= " @ " . $skycond_setup[1] . " feet";
}
$temp_f = (intVal($ready_to_parse->temp_c) * 1.8) + 32;
$dewpoint_c = $ready_to_parse->dewpoint_c;
$dewpoint_f = (intVal($ready_to_parse->dewpoint_c) * 1.8) + 32;
$wind = $ready_to_parse->wind_speed_kt . "kt @ " . $ready_to_parse->wind_dir_degrees . "°";
// Bitbar content echo

$bitbar_content = $temp_f . "°F";
$bitbar_content .= "\n---";
$bitbar_content .= "\n Current information for " . $ICAO;
$bitbar_content .= "\nView Report From...";
$bitbar_content .= "\n--DarkSky|href=https://darksky.net/forecast/" . $ready_to_parse->latitude . "," . $ready_to_parse->longitude;
$bitbar_content .= "\n--National Weather Service|href=https://forecast.weather.gov/MapClick.php?lat=" . $ready_to_parse->latitude . "&lon=" . $ready_to_parse->longitude;
$bitbar_content .= "\nSky Condition: " . $skycond_ready;
$bitbar_content .= "\nVisibility: " . $ready_to_parse->visibility_statute_mi . " miles";
$bitbar_content .= "\nPressure: " . $ready_to_parse->altim_in_hg . " mmHg";
$bitbar_content .= "\nDewpoint: ". $dewpoint_f . "°F";
$bitbar_content .= "\nWind: " . $wind;


echo $bitbar_content;
