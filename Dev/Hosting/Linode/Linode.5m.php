#!/usr/bin/php

<?php
// <xbar.title>Linode Status</xbar.title>
// <xbar.version>v1.0</xbar.version>
// <xbar.author>Brendon Cheves</xbar.author>
// <xbar.author.github>misfitius</xbar.author.github>
// <xbar.desc>Uses linode-cli to check the status of your Linodes.</xbar.desc>
// <xbar.dependencies>linode-cli,php</xbar.dependencies>

$status = true;
$json = shell_exec('/usr/local/bin/linode-cli linodes list --json');
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
