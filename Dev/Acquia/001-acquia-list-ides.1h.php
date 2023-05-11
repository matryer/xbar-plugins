#!/usr/bin/php
<?php

#  <xbar.title>Acquia Cloud IDEs</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Matthew Grasmick</xbar.author>
#  <xbar.author.github>grasmash</xbar.author.github>
#  <xbar.desc>List Acquia Cloud IDEs.</xbar.desc>
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
  ->setText('Acquia IDEs')
  ->show(TRUE);

$ides = acli("api:accounts:ide-list");
if (!is_array($ides)) {
  $mainMenu = $line
    ->setColor('red')
    ->setText('Could not get IDE list. Did you authenticate using Acquia CLI?')
    ->show();
  exit(0);
}

$mainMenu = $line
  ->setText('Visit Acquia Cloud')
  ->setUrl('https://cloud.acquia.com/a')
  ->show(TRUE);

foreach ($ides as $ide) {
  // Set the text and formatting
  $mainMenu = $line
    ->setText($ide->label);

  $mainMenu = $mainMenu->addSubMenu()
    ->newLine()
    ->setUrl($ide->_links->ide->href)
    ->setText('Open IDE');

  $mainMenu = $mainMenu->addSubMenu()
    ->newLine()
    ->setUrl($ide->_links->web->href)
    ->setText("Open IDE's Drupal site");

  $mainMenu = $mainMenu->addSubMenu()
    ->newLine()
    ->setUrl(str_replace('/api', '/a', $ide->_links->application->href))
    ->setText('Visit parent Cloud app');

  $mainMenu = $mainMenu->addSubMenu()
    ->newLine()
    ->setUrl($ide->_links->self->href)
    ->setText("View IDE API endpoint");

  $mainMenu = $mainMenu->addSubMenu()
    ->newLine()
    ->setBash(ACLI_PATH,['ide:delete' , $ide->uuid, '--no-interaction'])
    ->setTerminal(TRUE)
    ->setColor('red')
    ->setText('Delete IDE');

  $mainMenu->show();
}

exit(0);