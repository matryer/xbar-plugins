#!/usr/bin/php
<?php

#  <xbar.title>Acquia Cloud Applications</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Matthew Grasmick</xbar.author>
#  <xbar.author.github>grasmash</xbar.author.github>
#  <xbar.desc>List Acquia Cloud Applications.</xbar.desc>
#  <xbar.dependencies>php,acli,composer,steveedson/bitbar-php</xbar.dependencies>

// @see https://github.com/matryer/xbar-plugins/blob/main/CONTRIBUTING.md
require __DIR__ . '/.acquia-includes.php';
use SteveEdson\BitBar;

checkPrerequisites();
// Create BitBar formatter
// @see https://github.com/SteveEdson/bitbar-php
$bb = new BitBar();
$line = $bb->newLine();
$mainMenu = $line
  ->setText('Acquia Apps')
  ->show(TRUE);

$apps = acli("api:applications:list --sort=name");
if (!is_array($apps)) {
  $mainMenu = $line
    ->setColor('red')
    ->setText('Could not get application list. Did you authenticate using Acquia CLI?')
    ->show();
  exit(0);
}

$mainMenu = $line
  ->setText('Visit Acquia Cloud')
  ->setUrl('https://cloud.acquia.com/a')
  ->show(true);

foreach ($apps as $app) {
  $mainMenu = $line
    ->setText($app->name)
    ->setUrl('https://cloud.acquia.com/a/applications/' . $app->uuid);

  $mainMenu = $mainMenu->addSubMenu()
    ->newLine()
    ->setUrl('https://cloud.acquia.com/a/applications/' . $app->uuid . '/environments')
    ->setText('View environments');

  $mainMenu = $mainMenu->addSubMenu()
    ->newLine()
    ->setUrl('https://cloud.acquia.com/api/applications/' . $app->uuid)
    ->setText('View API response');

  $mainMenu->show();
}

exit(0);