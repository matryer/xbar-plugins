#!/usr/bin/php
<?php
// <bitbar.title>Catchpoint Alerts</bitbar.title>
// <bitbar.version>v1.1</bitbar.version>
// <bitbar.author>Robert Castley</bitbar.author>
// <bitbar.author.github>rcastley</bitbar.author.github>
// <bitbar.desc>Display Catchpoint Alerts via the Pull API.</bitbar.desc>
// <bitbar.image>http://i.imgur.com/xgkKWjy.png</bitbar.image>
// <bitbar.dependencies>php, cURL</bitbar.dependencies>
// <bitbar.abouturl>http://www.catchpoint.com/</bitbar.abouturl>
echo "| templateImage=iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAABrklEQVQ4jYXTy2oUURAG4G96ro7xmhARERHcZm3AhUJAxIW4Fl/B13Onj6EoatCgIJF4jZO59IyL+TvTMyFY0Jw6df3rr9OcLjdzPsDOiu2ENFfuLWxigkc4hz/oYR3beIMNDDA7rVAfj7GGV/EfYIQj/EijO3gb+4lCO7iI90F0kHNUO38l9jBot/ABipWxGkn6irMYx1fG18IQe9G7dURd3E/wx5zddC0xTfEynBS1SUpcx35RC94PR238TdIs3Ayjlxmxgd/mhN9A0cLlBA/yVd1m+F4jtBMOqwadNNjFrMCTFDhT46hCcryV6MPEjHPvZdRnjQTdDfttiw0N8NOyXEjDdpBPcQsvm7idgNJ8vRVvzRScpkjbfOWNjCWj9tFvBm5hvs5RrVAj0KvNnrd4AlXeeu5fCnyLYy3GifkbKpLYzyf+VvQr4XIPn6sH2Qy5G7hkvv5+DVEz3BQW76gTRJvSsZI+HuJ1RqvezjQoxhlxgmtB/SKxS/9aK2SOgqqRcY9ih6uxj2LbTaOlQpPMe4h7+JTEdgr34tvGc7yz2Oh/5Wk42Iq+Ssex/AOGoHohWJhtKAAAAABJRU5ErkJggg==\n";
echo "---\n";

$key    = 'Catchpoint_Key';
$secret = 'Catchpoint_Secret';

$data = array('grant_type' => 'client_credentials', 'client_id' => $key, 'client_secret' => $secret);

$ch = curl_init();

curl_setopt($ch, CURLOPT_URL, 'https://io.catchpoint.com/ui/api/token');
curl_setopt($ch, CURLOPT_POSTFIELDS, http_build_query($data));    
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

$result = curl_exec($ch);

curl_close($ch);

$result = json_decode($result);

$_SESSION['token'] = base64_encode($result->access_token);

$ch = curl_init();

curl_setopt($ch, CURLOPT_URL, 'https://io.catchpoint.com/ui/api/v1/alerts?pageSize=5');
curl_setopt($ch, CURLOPT_HTTPHEADER, 
    array('Authorization: Bearer ' . $_SESSION['token']));

curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

$data = curl_exec($ch);

curl_close($ch);

$alerts = json_decode($data);

if ($alerts != null) {
	foreach ($alerts->alerts as $a) {
		switch ($a->level->id) {
			case 0:
				$color = '#ff8000';
				$state = 'Warning';
				$emoji = ':neutral_face:';
				break;
			case 1:
				$color = '#ff0000';
				$state = 'Critical';
				$emoji = ':worried:';
				break;
			case 3:
				$color = '#80ff00';
				$state = 'OK';
				$emoji = ':smile:';
				break;
		}
		echo ":clock1030: " . $a->report_time . " | size=10 color=" . $color. "\n";
		echo $emoji . " Test: " . $a->test->name . " - " . $state . "| size= 10 color=" . $color . " href=http://portal.catchpoint.com/ui/Content/Charts/Performance.aspx?tList=" . $a->test->id . "&z=&chartView=1\n";
		echo "Reason: " . $a->alert_type->name . " | size=10 color=#000000\n";
		echo "---\n";
	}
} else {
	echo ":smile: No recent alerts | size=14 color=\"#80ff00\"";
}
//echo exec("osascript -e 'display notification \"" . $a->report_time . " - " . $a->alert_type->name . "\" with title \"" . $a->test->name . " - " . $state . "\"'");
