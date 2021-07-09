#!/usr/bin/env php

<?php
# <bitbar.title>Aare.guru</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Christian Studer</bitbar.author>
# <bitbar.author.github>cstuder</bitbar.author.github>
# <bitbar.desc>Displays current Aare river temperature</bitbar.desc>
# <bitbar.image>https://aare.guru/ios/gurulogo.svg</bitbar.image>
# <bitbar.dependencies>php</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/Aareguru/aare.guru.bitbar</bitbar.abouturl>

// Configuration
$schmerzgrenze = 18; // [°C], switches color of the display when reached
$city = "bern"; // For a list of all possible cities look here: https://aareguru.existenz.ch/v2018/cities

// Internal configuration
$version = '1.1';
$app = 'aare.guru.bitbar';
$coldcolor = '#0000a0';
$warmcolor = '#009fb0';
$coldcolor_darkmode = '#d1d1e0';
$warmcolor_darkmode = '#00cc00';
$apiurl = "https://aareguru.existenz.ch/v2018/today?city={$city}&app={$app}&version={$version}";

// Fetch data the oldfashioned way
$raw = @file_get_contents($apiurl);
$data = @json_decode($raw);

if (!$data) {
    echo "?°\n";
    echo "---\n";
    echo "Tschuldigung.\n";
    echo "Keine Antwort vom Aare.guru. :-(|href=https://aare.guru/#{$city}";
    exit(1);
}

// Display data
$temperature = $data->aare;
$text = $data->text;
$longname = $data->longname;

$ago = time() - $data->time;
$agostring = '';

if ($ago < 120) {
    $agostring = "vor {$ago} Sekunden";
} else if ($ago < (60 * 60)) {
    $minutes = floor($ago / 60);
    $agostring = "vor {$minutes} Minuten";
} else {
    $agostring = "vor längerer Zeit";
}

// Determine color
$darkmode = getenv('BitBarDarkMode');

if ($temperature >= $schmerzgrenze) {
    $color = $darkmode ? $warmcolor_darkmode : $warmcolor;
} else {
    $color = $darkmode ? $coldcolor_darkmode : $coldcolor;
}

// Output
echo "{$temperature}°|color={$color}\n";
echo "---\n";
echo "Gemessen {$agostring} in {$longname}\n";
echo "{$text}|href=https://aare.guru/#{$city}";
