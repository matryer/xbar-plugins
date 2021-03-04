#!/usr/bin/env  php
<?php
# <bitbar.title>RabbitMQ Status</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>Yann Milin</bitbar.author>
# <bitbar.author.github>ymilin</bitbar.author.github>
# <bitbar.desc>Keep an eye on your RabbitMQ server status from your menu bar !</bitbar.desc>
# <bitbar.image>http://i.imgur.com/Gz8FCJL.png</bitbar.image>
# <bitbar.dependencies>php >= 5.4.0</bitbar.dependencies>
# 
# RabbitMQ management plugin must be activated on server https://www.rabbitmq.com/management.html
# RabbitMQ Management HTTP API documentation https://raw.githack.com/rabbitmq/rabbitmq-management/rabbitmq_v3_6_0/priv/www/api/index.html

// Config
$baseUrl = "http://example.com";
$port = "15672";
$user = "guest";
$password = "guest";
$queuedMessageCountErrorLimit = 1000; // alert if queued messages higher than this, 0 = not active
// Config end

$statusBlackCircle = "⚫️";
$statusRedCircle = "🔴";
$statusWarning = "⚠️";
$colorBlack = "#333333";
$colorRed = "#FF0000";

$headers = [
    "Authorization: Basic " . base64_encode("$user:$password"),
    "content-type: application/json",
    "Accept: application/json",
    "Cache-Control: no-cache",
    "Pragma: no-cache",
];
$ch = curl_init();
$errors = [];

$uriOverview = "/api/overview";
$uriNodes = "/api/nodes";
$uriQueues = "/api/queues";

curl_setopt($ch, CURLOPT_PORT, $port);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
curl_setopt($ch, CURLOPT_TIMEOUT, 60);
curl_setopt($ch, CURLOPT_HTTPHEADER, $headers);

/**
 * Executes a GET request and returns response body in array format
 *
 * @param $ch
 * @param $url
 * @return mixed
 */
$curl_exec = function (&$ch, $url) {
    curl_setopt($ch, CURLOPT_URL, $url);
    return json_decode(curl_exec($ch), true);
};

/**
 * Handles error message from the api, stopping the script on errors
 *
 * @param $ch
 * @param $response
 */
$curlHandleResponseError = function (&$ch, $response) {
    global $statusWarning, $colorRed;
    if (!$response) {
        $error = curl_error($ch);
        echo "$statusWarning Error |color=$colorRed\n";
        echo "---\n";
        echo "$error | color=$colorRed\n";
        exit;
    }
    if (array_key_exists('error', $response)) {
        echo "$statusWarning Error |color=$colorRed\n";
        echo "---\n";
        echo "error: {$response['error']}, reason: {$response['reason']} | color=$colorRed\n";
        exit;
    }
};

// api calls
$responseOverview = $curl_exec($ch, $baseUrl . $uriOverview);
$curlHandleResponseError($ch, $responseOverview);
$responseNodes = $curl_exec($ch, $baseUrl . $uriNodes);
$curlHandleResponseError($ch, $responseNodes);
$responseQueues = $curl_exec($ch, $baseUrl . $uriQueues);
$curlHandleResponseError($ch, $responseQueues);

// Extract values from responses
$messagesReady = $responseOverview['queue_totals']['messages_ready'];
$messagesUnacked = $responseOverview['queue_totals']['messages_unacknowledged'];
$messagesTotal = $responseOverview['queue_totals']['messages'];
$connections = $responseOverview['object_totals']['connections'];
$channels = $responseOverview['object_totals']['channels'];
$exchanges = $responseOverview['object_totals']['exchanges'];
$queuesCount = $responseOverview['object_totals']['queues'];
$consumers = $responseOverview['object_totals']['consumers'];
$rabbitMqVersion = $responseOverview['rabbitmq_version'];
$erlangVersion = $responseOverview['erlang_version'];

$statusIcon = $statusBlackCircle;
$nodes = [];
foreach ($responseNodes as $responseNode) {
    $nodes[] = [
        'name' => $responseNode['name'],
        'running' => $responseNode['running'],
    ];

    if (!$responseNode['running']) {
        $statusIcon = $statusRedCircle;
        $errors[] = "RabbitMQ is reporting that node {$responseNode['name']} is not running correctly, known VPN issue on server.";
    }
}

$queues = [];
foreach ($responseQueues as $responseQueue) {
    $queues[] = [
        'name' => $responseQueue['name'],
        'state' => $responseQueue['state'],
    ];

    if ($responseQueue['state'] !== "running") {
        $statusIcon = $statusRedCircle;
        $errors[] = "RabbitMQ is reporting that queue {$responseQueue['name']} is not running correctly.";
    }
}


if ($queuedMessageCountErrorLimit && intval($messagesReady) > $queuedMessageCountErrorLimit) {
    $statusIcon = $statusRedCircle;
    $errors[] = "Number of message in queue higher than $queuedMessageCountErrorLimit, please check consumers status.";
}

// Top menu : status icon + count message ready
echo $statusIcon . " " . $messagesReady . "|color=$colorBlack\n";
echo "---\n";
// Error messages
foreach ($errors as $error) {
    echo "$error | color=$colorRed\n";
}
if ($errors) {
    echo "---\n";
}
// Message count detail
echo "Queued messages\n";
echo "Ready: $messagesReady | color=$colorBlack\n";
echo "Unacked: $messagesUnacked | color=$colorBlack\n";
echo "Total: $messagesTotal | color=$colorBlack\n";
echo "---\n";
// Global count for Connections, Channels, Exchanges, Queues, Consumers:
echo "Global counts\n";
echo "Connections: $connections | color=$colorBlack\n";
echo "Channels: $channels | color=$colorBlack\n";
echo "Exchanges: $exchanges | color=$colorBlack\n";
echo "Queues: $queuesCount | color=$colorBlack\n";
echo "Consumers: $consumers | color=$colorBlack\n";
echo "---\n";
// Nodes status
echo "Nodes\n";
foreach ($nodes as $node) {
    $nodeStatus = $node['running'] ? "ok" : "error";
    $nodeStatusColor = $node['running'] ? $colorBlack : $colorRed;
    echo "{$node['name']} status $nodeStatus | color=$nodeStatusColor\n";
}
echo "---\n";
echo "Queues\n";
foreach ($queues as $queue) {
    $queueStatusColor = $queue['state'] === "running" ? $colorBlack : $colorRed;
    echo "{$queue['name']} status {$queue['state']} | color=$queueStatusColor\n";
}
echo "---\n";
// manual refresh + RabbitMQ version TAG + management interface link
echo "RabbitMQ v$rabbitMqVersion, Erlang $erlangVersion\n";
echo "$baseUrl:$port | href=$baseUrl:$port \n";
echo "Refresh | refresh=true \n";

curl_close($ch);
