#!/usr/bin/php

<?php
// <bitbar.title>Linode Status</bitbar.title>
// <bitbar.version>v1.0</bitbar.version>
// <bitbar.author>Brendon Cheves</bitbar.author>
// <bitbar.author.github>misfitius</bitbar.author.github>
// <bitbar.desc>Uses linode-cli to check the status of your Linodes.</bitbar.desc>
// <bitbar.dependencies>linode-cli,php</bitbar.dependencies>

$status = true;
$json = shell_exec('/usr/local/bin/linode list -j');
$servers = json_decode($json);
$output = '---' . "\n";
foreach($servers as $server)
{
	$status &= ($server->status == 'running');
	$output .= $server->label . ' : ' . $server->status . '|color=' . ($server->status == 'running' ? 'green' : 'red') . "\n";
}
$output .= 'Go to Linode account|href=https://manager.linode.com';

echo 'Linode|color=' . ($status ? 'green' : 'red') . "\n" . $output;
?>