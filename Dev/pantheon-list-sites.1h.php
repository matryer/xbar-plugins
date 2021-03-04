#!/usr/bin/php
<?php

// <bitbar.title>Pantheon - List Sites</bitbar.title>
// <bitbar.version>v1.0</bitbar.version>
// <bitbar.author>Dave Wikoff</bitbar.author>
// <bitbar.author.github>derimagia</bitbar.author.github>
// <bitbar.desc>List and manage all of your sites you are on in Pantheon.</bitbar.desc>
// <bitbar.dependencies>php, terminus</bitbar.dependencies>
// <bitbar.image>https://i.imgur.com/VYBizXY.png</bitbar.image>
// <bitbar.abouturl>https://github.com/derimagia/pantheon-bitbar</bitbar.abouturl>

define('TERMINUS_PATH', '/usr/local/bin/terminus');
define('CONFIG_PATH', '/tmp/pantheon-list-sites-config.json');
define('DEBUG_MODE', false);

$php = PHP_BINARY;
$script = escapeshellarg($argv[0]);
$directory = dirname(__FILE__);
$html_filename = pathinfo(__FILE__, PATHINFO_FILENAME) . '.dynamic.html';
$html_filepath = $directory . '/' . $html_filename;
$config = get_config();
$env_id = $config->env_id ? $config->env_id : 'dev';

if (!empty($argv[1]) && function_exists($argv[1])) {
  $args = $argv;
  // Shift the first 2 arguments
  array_shift($args);
  array_shift($args);
  call_user_func_array($argv[1], $args);
  exit(0);
}

$sites = terminus("sites list --cached");

if (!is_array($sites)) {
  echo 'Could not get site list. Did you auth using Terminus?';
  exit();
}

$symbolMap = [
  'dev' => '🔵',
  'test' => '⚫',
  'live' => '🔴',
];

$symbol = isset($symbolMap[$env_id]) ? $symbolMap[$env_id] : '';

$items = array(
  ['title'  => "Environment: $env_id -- $symbol", 'bash' => $php, 'param1' => $script, 'param2' => 'pantheon_switch_environment', 'param3' => $env_id, 'terminal' => 'false', 'refresh' => 'true'],
  '---',
);

foreach ($sites as $site) {
  $items[] = ['title' => $site->name, 'bash' => $php, 'param1' => $script, 'param2' => 'pantheon_open_site', 'param3' => $site->name, 'param4' => $env_id, 'terminal' => 'false'];
  if ($site->framework === 'drupal') {
    $items[] = ['title' => "$site->name -- 🔒", 'alternate' => 'true', 'bash' => $php, 'param1' => $script, 'param2' => 'drush_user_login', 'param3' => $site->name, 'param4' => $env_id, 'terminal' => 'true'];
  }
  $items[] = ['title' => '└ Pantheon Dashboard -- ⚡', 'bash' => $php, 'param1' => $script, 'param2' => 'pantheon_open_dashboard', 'param3' => $site->name, 'param4' => $env_id, 'terminal' => 'true'];
  $items[] = '---';
}

echo "⚡\n";
echo "---\n";

foreach ($items as $item) {
  if (is_array($item)) {
    $parts = [];
    foreach ($item as $param => $value) {
      $parts[] = $param . '="' . $value . '"';
    }
    $item = $item['title'] . ' | ' . implode(' ', $parts);
  }

  echo $item . "\n";
}
exit(0);

/**
 * Open the dashboard for a site
 */
function pantheon_open_dashboard($site_id) {
  return browser_open(terminus("site dashboard --print", $site_id));
}

/**
 * Login to the site as User 1 for Drupal
 */
function drush_user_login($site_id, $env_id) {
  $login_url = drush($site_id, $env_id, "user-login 1");
  return browser_open($login_url);
}

/**
 * Switch the environment
 */
function pantheon_switch_environment($current_env_id) {
  global $script, $env_id, $config;

  $environments = ['dev', 'test', 'live'];

  foreach ($environments as $env) {
    if ($env == $current_env_id) {
      break;
    }
  }

  $next_env = current($environments) ? current($environments) : 'dev';

  $config->env_id = $next_env;
  save_config($config);
}

/**
 * Returns the domain for a pantheon site.
 */
function pantheon_open_site($site_id, $env_id) {
  $alias = drush_get_alias($site_id, $env_id);

  $url = sprintf('%s://%s', 'https', $alias['uri']);

  return browser_open($url);
}

/**
 * Gets a Drush Alias for a Site ID / Env ID
 *
 * @return bool|array
 */
function drush_get_alias($site_id, $env_id) {
  $phpcode = terminus('sites aliases --print', NULL, NULL, FALSE);
  /* @var $aliases array[] */
  eval($phpcode);

  if (empty($aliases[$site_id . '.' . $env_id])) {
    echo 'Invalid Alias';
    exit(1);
  }

  return $aliases[$site_id . '.' . $env_id];
}

/**
 * Get a list of Pantheon Environments
 */
function pantheon_get_envs($site_id) {
  return terminus('site environments', $site_id);
}

/**
 * Opens a URL in the browser
 */
function browser_open($url) {
  passthru("open $url", $return_var);
  return $return_var;
}

/**
 * Run a terminus command
 * @return object
 */
function terminus($command, $site_id = null, $env_id = null, $json = TRUE) {
  $extras = ' --yes';
  if (!empty($json)) $extras .= ' --format=json';
  if (!empty($site_id)) $extras .= ' --site=' . $site_id;
  if (!empty($env_id)) $extras .= ' --env=' . $env_id;

  $command = TERMINUS_PATH . " {$command}{$extras}";

  $output = passthrough_return($command);
  return $json ? json_decode($output) : $output;
}

/**
 * We need to manually call drush because we need to add custom SSH options that terminus doesn't support.
 *
 * @return bool
 */
function drush($site_id, $env_id, $drush_command) {
  $alias = drush_get_alias($site_id, $env_id);
  $remote_host = $alias['remote-host'];
  $remote_user = $alias['remote-user'];
  $ssh_options = $alias['ssh-options'] . ' -o "StrictHostKeyChecking=no" -o "UserKnownHostsFile=/dev/null"';

  $command = escapeshellarg('drush ' . $drush_command);
  $command = 'ssh -T ' . $remote_user . '@' . $remote_host . ' ' . $ssh_options . ' ' . $command;

  return passthrough_return($command);
}

/**
 * Pass's the command through and returns it
 */
function passthrough_return($command) {
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

/**
 * Gets the config for this plugin
 */
function get_config() {
  if (file_exists(CONFIG_PATH)) {
    $config = json_decode(file_get_contents(CONFIG_PATH));
  }
  return !empty($config) ? $config : new stdClass();
}

/**
 * Saves the config for this plugin
 */
function save_config($config) {
  return file_put_contents(CONFIG_PATH, json_encode($config));
}
