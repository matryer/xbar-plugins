<?php
require $_SERVER['HOME'] . "/.composer/vendor/autoload.php";

const ACLI_PATH = '/usr/local/bin/acli';
const DEBUG_MODE = false;

/**
 * Validates that ACLI, Composer, and steveedson/bitbar-php are installed.
 *
 * Returns early with error message if pre-requisite is not installed.
 */
function checkPrerequisites() {
  if (!file_exists('/usr/local/bin/acli')) {
    print "Error\n";
    print "---\n";
    print "Missing pre-requisite Acquia CLI\n";
    print "Read Install Instructions|color=red|href=https://github.com/matryer/xbar-plugins/blob/main/Dev/Acquia/INSTALL.md";
    exit(0);
  }
  if (!file_exists($_SERVER['HOME'] . '/.composer/vendor/autoload.php')) {
    print "Error\n";
    print "---\n";
    print "Missing pre-requisite Composer\n";
    print "Read Install Instructions|color=red|href=https://github.com/matryer/xbar-plugins/blob/main/Dev/Acquia/INSTALL.md";
    exit(0);
  }
  if (!file_exists($_SERVER['HOME'] . '/.composer/vendor/steveedson/bitbar-php')) {
    print "Error\n";
    print "---\n";
    print "Missing pre-requisite steveedson/bitbar-php\n";
    print "Read Install Instructions|color=red|href=https://github.com/matryer/xbar-plugins/blob/main/Dev/Acquia/INSTALL.md";
    exit(0);
  }
}

/**
 * Run an acli command.
 *
 * @return object
 *   A decoded JSON response.
 */
function acli($command)
{
  $extras = ' --no-interaction';
  $command = ACLI_PATH . " {$command}{$extras}";

  $output = passthrough_return($command);
  return json_decode($output);
}

/**
 * Executes a command via passthru. Grabs output.
 *
 * @return string
 *   The command output.
 */
function passthrough_return($command)
{
  ob_start();

  if (!DEBUG_MODE) {
    $command = $command . ' 2>/dev/null';
  }

  passthru($command);
  $output = ob_get_clean();

  if (DEBUG_MODE) {
    echo "----- DEBUG [$command] -----\n";
    echo "OUTPUT:\n";
    var_dump($output);
    echo "\n";
  }

  return $output;
}